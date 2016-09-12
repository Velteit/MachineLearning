open System
open System.IO
open System.Runtime.InteropServices


module FileStream = 
    let inline read i size (fs: FileStream) = 
        let buffer = Array.zeroCreate size
        fs.Seek(i * int64 size, SeekOrigin.Begin) |> ignore
        fs.Read(buffer, 0, size) |> ignore 
        buffer

    let inline write buffer index size (fs: FileStream) = 
        fs.Seek(index * int64 size, SeekOrigin.Begin) |> ignore
        fs.Write(buffer, 0, size)
        fs.Flush()
    
    let inline copyBytes output start end' size (inputStream: FileStream) = 
        use outputStream = new FileStream(output, FileMode.CreateNew)
        for i in start..end' do
            let buffer = read (i * int64 size) size inputStream
            write buffer i size outputStream

let inline mergeSort<'T when 'T : comparison> (path: string) (converter: byte [] -> 'T) = 
    let size = Marshal.SizeOf<'T>() 
    let sizeLong = size |> int64
    let tempFiles = System.Collections.Generic.List<string>()
    let addTempMarker file = 
        Path.Combine(Path.GetDirectoryName(file), 
            Path.GetFileNameWithoutExtension(file) + "-" + (Guid.NewGuid().ToString()) + Path.GetExtension(file))
    let split (fs: FileStream) count = 
        let left = addTempMarker path
        let right = addTempMarker path
        FileStream.copyBytes left 0L (count / 2L) size fs
        FileStream.copyBytes right (count / 2L + 1L) (count - 1L) size fs
        tempFiles.Add left
        tempFiles.Add right
        left, right

    let merge (left: string) (right: string) = 
        let merged = addTempMarker path
        use mergedStream = new FileStream(merged, FileMode.CreateNew)
        use leftStream = new FileStream(left, FileMode.Open)
        use rightStream = new FileStream(right, FileMode.Open)
        let rec inner i j c = 
            if not leftStream.CanRead && rightStream.CanRead then 
                let rb = FileStream.read j size rightStream
                FileStream.write rb c size mergedStream
                inner i (j  + 1L) (c + 1L)
            if not rightStream.CanRead && leftStream.CanRead then 
                let lb = FileStream.read i size leftStream
                FileStream.write lb c size mergedStream
                inner (i + 1L) j (c + 1L)
            if rightStream.CanRead && leftStream.CanRead then
                let lb = FileStream.read i size leftStream
                let rb = FileStream.read j size rightStream
                if converter lb <  converter rb then
                    FileStream.write lb c size mergedStream
                    inner (i + 1L) j (c + 1L)
                else 
                    FileStream.write rb c size mergedStream
                    inner i (j + 1L) (c + 1L)
        inner 0L 0L 0L
        merged
    let rec inner path =
        async {
            use fs = File.OpenRead(path)
            let count = fs.Length/sizeLong
            if count <= 1L then 
                return path
            else
                let left, right = split fs count
                return merge (inner left |> Async.RunSynchronously) (inner right |> Async.RunSynchronously)
        }
    try
        inner path |> Async.RunSynchronously
    finally
        for file in tempFiles do
            File.Delete file
            

    
let bytesToInt (buffer : byte []) = 
    buffer 
    |> Array.mapi (fun i b -> int b * pown 2 i) 
    |> Array.sum

let rgen = System.Random()
let path = 
    Path.Combine(Path.GetTempPath(), "bigsort")
    |> fun path -> 
        if (not << Directory.Exists) path then Directory.CreateDirectory(path) |> ignore
        Path.Combine(path, sprintf "sort.bin")

let create path count = 
    if File.Exists path then File.Delete path
    use fs = new FileStream(path, FileMode.OpenOrCreate)
    for i in 0L..(count - 1L) do 
        let buffer = Array.zeroCreate 4
        rgen.NextBytes(buffer)
        fs.Seek(i * 4L, SeekOrigin.Begin) |> ignore
        fs.Write(buffer, 0, 4)
        fs.Flush()


let printFile path count converter = 
    use fs = new FileStream(path, FileMode.OpenOrCreate)
    for i in 1L..count do 
        let v = FileStream.read i 4 fs
        printfn "%d" (converter v)


create path 10L
printFile path 10L bytesToInt
mergeSort<int> path (bytesToInt) 


