module DataStructures

open System.Collections.Generic

type BinaryTree<'T when 'T : comparison> =
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>


type MinHeap<'T, 'G when 'G : comparison> (key: 'T -> 'G) =
    let mutable heap = List<'T>(1000)
    
    let item i = key (heap[i])

    // Swap two elements in the heap
    let swap i j =
        let temp = heap.[i]
        heap.[i] <- heap.[j]
        heap.[j] <- temp

    // Bubble up to maintain the heap property
    let rec bubbleUp index =
        if index > 0 then
            let parentIndex = (index - 1) / 2
            if item index < item parentIndex then
                swap index parentIndex
                bubbleUp parentIndex

    // Bubble down to maintain the heap property
    let rec bubbleDown index =
        let leftChildIndex = 2 * index + 1
        let rightChildIndex = 2 * index + 2
        let size = heap.Count

        let smallest1 =
            if leftChildIndex < size && item leftChildIndex < item index then
                leftChildIndex
            else
                index

        let smallest2 = 
            if rightChildIndex < size && item rightChildIndex < item smallest1 then
                rightChildIndex
            else
                smallest1

        if smallest2 <> index then
            swap index smallest2
            bubbleDown smallest2

    member this.Insert(value: 'T) =
        heap.Add value
        bubbleUp (heap.Count - 1)

    member this.RemoveMin() =
        let minValue = heap.[0]
        let lastValue = heap[heap.Count - 1]

        swap 0 (heap.Count - 1)
        heap.RemoveAt(heap.Count - 1)

        bubbleDown 0            
        minValue

    member this.Peek() = heap.[0]

    member this.Size() = heap.Count

    member this.IsEmpty() = heap.Count = 0