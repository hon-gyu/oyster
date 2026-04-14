open Core
open Cmarkit
open Parse.Struct
open Parse.Struct.For_test
module Theme = Oystermark.Theme

let html_of_doc = Component.Html.For_test.html_of_doc

let example =
  {|
Architecture:
- : encoder–decoder. each half is a stack of six identical layers. self-attention and position-wise feed-forward replace recurrent and convolutional layers
- encoder:
  - self-attention: multi-head, every position attends to every other input position
  - feed-forward: position-wise MLP, applied independently at each step
  - residual + norm: a residual connection wraps each sub-layer, followed by layer normalization
- decoder:
  - masked self-attention sub-layer:
    - like the encoder's, but autoregressive
    - position `i` can only attend to positions `≤ i`
  - cross-attention: multi-head attention over the encoder output.
  - feed-forward sub-layer: same as on the encoder side
  - residual + norm:
    - as in the encoder

Attention:
- scaled dot-product:
  - definition: `softmax(QKᵀ / √dₖ) V`
  - `√dₖ`:
    - dot products grow like `√dₖ` as the key dimension grows. dividing keeps the softmax out of its saturation regime, so gradients don't vanish
- multi-head:
  - procedure:
    - run `h = 8` attention functions in parallel over learned linear projections of `Q`, `K`, `V`.
    - concatenate the heads and project.
  - why multiple heads:
    - each head can attend to a different representation subspace. a single softmax over averaged features cannot recover this.
|}
;;

let () =
  let doc = doc_of_string example in
  let section label style =
    sprintf "<hr><p><b>%s</b></p>\n%s" label (html_of_doc style doc)
  in
  let body =
    String.concat
      ~sep:"\n"
      [ section "plain" `Plain; section "basic" `Basic; section "graph" `Graph ]
  in
  let page : Theme.page =
    { title = "Struct"; body; url_path = ""; nav = ""; sidebar = "" }
  in
  Theme.default page |> print_string
;;
