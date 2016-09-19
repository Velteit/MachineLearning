(* Install EmguCV and set the path to the installation *)

#I @"C:\Emgu\emgucv-windesktop 3.1.0.2282\bin"

#r @"Emgu.CV.UI.dll"
#r @"Emgu.CV.UI.GL.dll"
#r @"Emgu.CV.World.dll"
#r "System.Windows.Forms"



open System
open System.Drawing
open System.Windows.Forms
open Emgu.CV
open Emgu.CV.CvEnum
open Emgu.CV.Structure
open Emgu.CV.Capture

let name = "Test Window"
let img = new Mat(200,400, DepthType.Cv8U, 3)
img.SetTo(Bgr(255., 0., 0.).MCvScalar)
CvInvoke.PutText(img, "Hello World!", Point(10, 80), FontFace.HersheyComplex,1., Bgr(0.,255.,0.).MCvScalar)
CvInvoke.Imshow(name, img)
CvInvoke.WaitKey 0
CvInvoke.DestroyWindow name

 
 
let capture = new Capture("test.mp4")

Application.Idle.Add(fun _ -> CvInvoke.Imshow("Camera", capture.QueryFrame()))