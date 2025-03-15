open Vortice.Vulkan
open type Vortice.Vulkan.Vulkan

(* utils *)

/// Check the given Vulkan operation result, logging on non-Success.
let check (result : VkResult) =
    if int result > 0 then printfn "Vulkan info: %s" (string result)
    elif int result < 0 then printfn "Vulkan assertion failed due to: %s" (string result)

(* main program *)

// load vulkan
vkInitialize () |> check
