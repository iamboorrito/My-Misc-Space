
open System.Threading.Tasks

(* Parallel scalar multiplication of vector *)
let ScaleVector (c: float) (v: float []) = 
    Array.Parallel.map (fun (x:float) -> c*x) v

(* Parallel vector addition *)
let AddVector (u: float[] ) (v: float[] ) = 
    let result:float[] = Array.zeroCreate u.Length
    Parallel.For(0, u.Length, (fun i -> 
        result.[i] <- u.[i]+v.[i]
    )) |> ignore
    result

(* Parallel Dot Product of two Arrays *)
let Dot (u: float []) (v: float []) = 
    (* First create result Array *)
    let result:float [] = Array.zeroCreate u.Length
    (* Parallel.For from i = 0 -> u.Length to set each element and ignore Parallel output *)
    Parallel.For(0, u.Length, (fun i -> 
        result.[i] <- u.[i]*v.[i]
    )) |> ignore
    (* Return result *)
    Array.sum result    

(* Matrix-Vector multiplication by sum of weighted columns *)
let mvMulByCol (M: float[,]) (v: float[]) =
    let mutable result:float[] = Array2D.length1 M |> Array.zeroCreate 
    // For each col, scale v[i]*col[i] and sum results
    for i = 0 to v.Length-1 do
        result <- AddVector result (ScaleVector v.[i] M.[*,i])
    result

(* Matrix-Vector multiplication by dot products *)
let mvMulByDot (M: float[,]) (v: float[]) =
    let result:float[] = Array2D.length1 M |> Array.zeroCreate
    (* Build answer one component at a time *)
    Parallel.For(0, Array2D.length1 M, (fun i ->
        result.[i] <- Dot M.[i, *] v
    )) |> ignore
    result

(* Sample matrix and vector *)
let m:float[,] = array2D [| [|1.3; 4.6; 7.3|];
                            [|1.6; 5.3; 5.1|] |];;
let v:float[] = [| 1.8; 11.6; 8.3|]

(* Use this to pause console *)
//System.Console.Read |> ignore