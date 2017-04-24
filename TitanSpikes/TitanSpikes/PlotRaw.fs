#if COMPILED
module PlotRaw
#else
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open Helper
open MathHelper
open PlotHelper
open RP.Controls
open System.Windows.Media

let hostname = Remote "140.247.178.94:5900"
let nspikes = (getSize hostname @"/root/data/asheshdhawale/Data/Gandhar/635385150755642455/ChGroup_1/SpikeTimes" |> Async.RunSynchronously)/8L|>int
let indx = getSpikeTimes hostname @"/root/data/asheshdhawale/Data/Gandhar/635385150755642455/ChGroup_1/Clusters2.indx" 0UL -1


