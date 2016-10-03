#load "packages/FsLab/FsLab.fsx"

#load "Utils.CSV.fs"
#load "Utils.Math.fs"
#load "Utils.Misc.fs"
#load "Utils.Differential.fs"
#load "Trees.fs"
#load "Neural.fs"


open System
open System.IO

open MathNet
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Providers.LinearAlgebra.Mkl

let combine (paths: string []) = Path.Combine paths

let os = if Environment.OSVersion.Platform.ToString().Contains("nix") then "Linux" else "Win"
let platform = if Environment.Is64BitOperatingSystem then "x64" else "x86"
let osp = os + "-" + platform
let baseName = "MathNet.Numerics"
let path = combine [|__SOURCE_DIRECTORY__; "packages"|]

if Directory.Exists(combine [|path; baseName + ".OpenBLAS."+ os |]) then
  printfn "Using OpenBLAS"
  Control.NativeProviderPath <- combine [|path; baseName + ".OpenBLAS."+ os; "build"; platform |]
  Control.UseNativeOpenBLAS()
else
  printfn "Using MKL"
  Control.NativeProviderPath <- combine [|path; baseName + ".MKL."+ os + "-" + platform; "build"; platform |]
  Control.UseNativeMKL()
