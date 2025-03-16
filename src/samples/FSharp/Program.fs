open System
open System.Runtime.CompilerServices
open FSharp.NativeInterop
open Vortice.Vulkan
open type Vortice.Vulkan.Vulkan

(* handles *)

let mutable instance = Unchecked.defaultof<VkInstance>
let mutable physicalDevice = Unchecked.defaultof<VkPhysicalDevice>
let mutable device = Unchecked.defaultof<VkDevice>

(* utils *)

/// Null pointer.
let nullPtr =
    NativePtr.nullPtr

/// Convert a managed pointer to a typed native pointer.
let asPointer<'a when 'a : unmanaged> (managedPtr : byref<'a>) : nativeptr<'a> =
    let voidPtr = Unsafe.AsPointer<'a> &managedPtr
    NativePtr.ofVoidPtr<'a> voidPtr

/// Convert a fixed-size buffer to an array of given length.
let fixedBufferToArray<'a when 'a : unmanaged> length fixedBuffer =
    let mutable fixedBuffer = fixedBuffer
    let voidPtr = Unsafe.AsPointer &fixedBuffer
    let ptr = NativePtr.ofVoidPtr<'a> voidPtr
    let array = Array.zeroCreate<'a> length
    for i in 0 .. (length - 1) do array.[i] <- NativePtr.get ptr i
    array

/// Abstraction for native pointer pinning for arrays.
type ArrayPin<'a when 'a : unmanaged> private (handle : Buffers.MemoryHandle, ptr : nativeptr<'a>) =

    /// Create an ArrayPin for a given array.
    new (array : 'a array) =
        let handle = array.AsMemory().Pin()
        let ptr = NativePtr.ofVoidPtr<'a> handle.Pointer
        new ArrayPin<'a> (handle, ptr)

    /// The native pointer to the pinned array.
    member this.Pointer = ptr

    /// The nativeint to the pinned array.
    member this.NativeInt = NativePtr.toNativeInt ptr

    interface IDisposable with
        member this.Dispose () =
            handle.Dispose ()

/// Check the given Vulkan operation result, logging on non-Success.
let check (result : VkResult) =
    if int result > 0 then printfn "Vulkan info: %s" (string result)
    elif int result < 0 then printfn "Vulkan assertion failed due to: %s" (string result)

/// A manually allocated buffer for diagnostic purposes.
type ManualAllocatedBuffer =
    { Buffer : VkBuffer 
      Memory : VkDeviceMemory
      Mapping : nativeptr<voidptr> }

    static member private findMemoryType typeFilter properties =
        
        // get memory types
        let mutable memProperties = Unchecked.defaultof<VkPhysicalDeviceMemoryProperties>
        vkGetPhysicalDeviceMemoryProperties (physicalDevice, &memProperties)
        let memoryTypes = fixedBufferToArray<VkMemoryType> (int memProperties.memoryTypeCount) memProperties.memoryTypes

        // try find suitable memory type
        let mutable memoryTypeOpt = None
        for i in 0 .. (memoryTypes.Length - 1) do
            match memoryTypeOpt with
            | None -> if typeFilter &&& (1u <<< i) <> 0u && memoryTypes.[i].propertyFlags &&& properties = properties then memoryTypeOpt <- Some (uint i)
            | Some _ -> ()

        // fin
        match memoryTypeOpt with
        | Some memoryType -> memoryType
        | None -> printfn "Failed to find suitable memory type!"; 0u
    
    static member private createInternal uploadEnabled bufferInfo =

        // create buffer
        let mutable buffer = Unchecked.defaultof<VkBuffer>
        vkCreateBuffer (device, &bufferInfo, nullPtr, asPointer &buffer) |> check

        // get buffer memory requirements
        let mutable memRequirements = Unchecked.defaultof<VkMemoryRequirements>
        vkGetBufferMemoryRequirements (device, buffer, &memRequirements)

        // choose appropriate memory properties
        let properties =
            if uploadEnabled then VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ||| VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
            else VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

        // allocate memory
        let mutable info = VkMemoryAllocateInfo ()
        info.allocationSize <- memRequirements.size
        info.memoryTypeIndex <- ManualAllocatedBuffer.findMemoryType memRequirements.memoryTypeBits properties
        let mutable memory = Unchecked.defaultof<VkDeviceMemory>
        vkAllocateMemory (device, asPointer &info, nullPtr, &memory) |> check

        // bind buffer to memory
        vkBindBufferMemory (device, buffer, memory, 0UL) |> check

        // map memory if upload enabled
        let mutable mapping = Unchecked.defaultof<nativeptr<voidptr>>
        if uploadEnabled then vkMapMemory (device, memory, 0UL, VK_WHOLE_SIZE, VkMemoryMapFlags.None, mapping) |> check
        
        // make ManualAllocatedBuffer
        let manualAllocatedBuffer = 
            { Buffer = buffer
              Memory = memory
              Mapping = mapping }

        // fin
        manualAllocatedBuffer

    /// Create a manually allocated uniform buffer.
    static member createUniform size =
        let mutable info = VkBufferCreateInfo ()
        info.size <- uint64 size
        info.usage <- Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
        let allocatedBuffer = ManualAllocatedBuffer.createInternal true info
        allocatedBuffer
    
    /// Destroy a ManualAllocatedBuffer.
    static member destroy buffer =
        if buffer.Mapping <> nullPtr then Vulkan.vkUnmapMemory (device, buffer.Memory)
        Vulkan.vkDestroyBuffer (device, buffer.Buffer, nullPtr)
        Vulkan.vkFreeMemory (device, buffer.Memory, nullPtr)

(* main functions *)

/// Create the instance.
let createInstance () =
    let mutable info = VkInstanceCreateInfo ()
    vkCreateInstance (&info, nullPtr, &instance) |> check

/// Get the physical device.
let getPhysicalDevice () =
    let mutable deviceCount = 0u
    vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, nullPtr) |> check
    let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
    use devicesPin = new ArrayPin<_> (devices)
    vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, devicesPin.Pointer) |> check
    physicalDevice <- devices.[0]

/// Create the device.
let createDevice () =
    let mutable info = VkDeviceCreateInfo ()
    vkCreateDevice (physicalDevice, &info, nullPtr, &device) |> check

(* main program *)

// load vulkan
vkInitialize () |> check

// create instance
createInstance ()

// load instance commands
vkLoadInstanceOnly instance

// get physical device
getPhysicalDevice ()

// create device
createDevice ()

// load device commands
vkLoadDevice device

// create and allocate buffer
let buffer = ManualAllocatedBuffer.createUniform 1024
