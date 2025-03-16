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
