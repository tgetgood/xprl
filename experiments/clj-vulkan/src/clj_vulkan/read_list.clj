(ns clj-vulkan.read-list
  (:require [clj-vulkan.c-utils :as c])
  (:import org.lwjgl.PointerBuffer
           org.lwjgl.system.MemoryStack
           [org.lwjgl.vulkan
            VK11
            VkLayerProperties
            VkQueueFamilyProperties
            VkPhysicalDevice]))

(defn reducible-pbuffer [this]
  (reify clojure.lang.IReduce
    (reduce [_ f start]
      (let [capacity (.capacity this)]
        (loop [i   0
               ret start]
          (if (clojure.lang.RT/isReduced ret)
            (deref ret)
            (if (< i capacity)
              (recur (inc i) (.invoke f ret (.get this i)))
              ret)))))
    (reduce [_ f]
      (let [capacity (.capacity this)]
        (when (< 0 capacity)
          (loop [i   1
                 ret (.get this 0)]
            (if (clojure.lang.RT/isReduced ret)
              (deref ret)
              (if (< i capacity)
                (recur (inc i) (.invoke f ret (.get this i)))
                ret))))))))

(defn gcalloc
    "Allocates a pointerbuffer to read the results of `f`, reads the results and
    dumps them into a (heap allocated) vector."
  ;; Experimental.
  [f]
  (with-open [^MemoryStack stack (MemoryStack/stackPush)]
    (let [^ints count-ptr    (.mallocInt stack 1)]
      (f count-ptr nil)
      (let [num                 (.get count-ptr 0)
            ^PointerBuffer ptrs (.mallocPointer stack num)]
        (f count-ptr ptrs)
        (into [] (reducible-pbuffer ptrs))))))

(defn validation-layers
  "Returns set of all validation layers supported by this system."
  []
  (with-open [stack (MemoryStack/stackPush)]
    (let [c*   (.mallocInt stack 1)]
      (VK11/vkEnumerateInstanceLayerProperties c* nil)
      (let [c         (.get c* 0)
            layers* (VkLayerProperties/mallocStack c stack)]
        (VK11/vkEnumerateInstanceLayerProperties c* layers*)
        (into [] layers*)))))


(defn queue-families [^VkPhysicalDevice device]
  (with-open [stack (MemoryStack/stackPush)]
    (let [c* (.mallocInt stack 1)]
      (VK11/vkGetPhysicalDeviceQueueFamilyProperties device c* nil)
      (let [c (.get c* 0)
            f* (VkQueueFamilyProperties/mallocStack c stack)]
        (VK11/vkGetPhysicalDeviceQueueFamilyProperties device c* f*)
        (into [] f*)))))