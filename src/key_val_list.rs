#![allow(warnings)] // reason: WIP
/// Custom syntax parsing a list block as nested key-value pairs
/// #WIP
use crate::ast::Tree;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Tree;
    use insta::{assert_debug_snapshot, assert_snapshot};

    fn data() -> String {
        r#######"
Cluster1
- asf
    - asdfwdw
    - dasfw
- gdklj
- asdf

###### Cluster2
- asf
    - asdfwdw
    - dasfw
- gdklj
- asdf

---

Empty line between list items won't break the list

- asf
    - asdfwdw
    - dasfw

- gdklj
    - vcxoiu

    - qsdoij
- asdf

---

- asf
    - asdfwdw
    - dasfw
 some_text

- gdklj
    - vcxoiu

    - qsdoij
- asdf

"#######
            .to_string()
    }

    #[test]
    fn test_ambiguous_list() {
        let md = data();
        let tree = Tree::new(&md);
        assert_snapshot!(tree.root_node, @r#"
        Document [1..347]
          Paragraph [1..10]
            Text(Borrowed("Cluster1")) [1..9]
          List(None) [10..58]
            Item [10..42]
              Text(Borrowed("asf")) [12..15]
              List(None) [18..42]
                Item [18..30]
                  Text(Borrowed("asdfwdw")) [22..29]
                Item [32..42]
                  Text(Borrowed("dasfw")) [36..41]
            Item [42..50]
              Text(Borrowed("gdklj")) [44..49]
            Item [50..58]
              Text(Borrowed("asdf")) [52..56]
          Heading { level: H6, id: None, classes: [], attrs: [] } [58..74]
            Text(Borrowed("Cluster2")) [65..73]
          List(None) [74..122]
            Item [74..106]
              Text(Borrowed("asf")) [76..79]
              List(None) [82..106]
                Item [82..94]
                  Text(Borrowed("asdfwdw")) [86..93]
                Item [96..106]
                  Text(Borrowed("dasfw")) [100..105]
            Item [106..114]
              Text(Borrowed("gdklj")) [108..113]
            Item [114..122]
              Text(Borrowed("asdf")) [116..120]
          Rule [122..126]
          Paragraph [127..178]
            Text(Borrowed("Empty line between list items won")) [127..160]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [160..161]
            Text(Borrowed("t break the list")) [161..177]
          List(None) [179..255]
            Item [179..212]
              Paragraph [181..185]
                Text(Borrowed("asf")) [181..184]
              List(None) [187..212]
                Item [187..199]
                  Text(Borrowed("asdfwdw")) [191..198]
                Item [201..212]
                  Text(Borrowed("dasfw")) [205..210]
            Item [212..247]
              Paragraph [214..220]
                Text(Borrowed("gdklj")) [214..219]
              List(None) [222..247]
                Item [222..234]
                  Paragraph [226..233]
                    Text(Borrowed("vcxoiu")) [226..232]
                Item [236..247]
                  Paragraph [240..247]
                    Text(Borrowed("qsdoij")) [240..246]
            Item [247..255]
              Paragraph [249..254]
                Text(Borrowed("asdf")) [249..253]
          Rule [255..259]
          List(None) [260..347]
            Item [260..304]
              Paragraph [262..266]
                Text(Borrowed("asf")) [262..265]
              List(None) [268..304]
                Item [268..280]
                  Text(Borrowed("asdfwdw")) [272..279]
                Item [282..304]
                  Text(Borrowed("dasfw")) [286..291]
                  SoftBreak [291..292]
                  Text(Borrowed("some_text")) [293..302]
            Item [304..339]
              Paragraph [306..312]
                Text(Borrowed("gdklj")) [306..311]
              List(None) [314..339]
                Item [314..326]
                  Paragraph [318..325]
                    Text(Borrowed("vcxoiu")) [318..324]
                Item [328..339]
                  Paragraph [332..339]
                    Text(Borrowed("qsdoij")) [332..338]
            Item [339..347]
              Paragraph [341..346]
                Text(Borrowed("asdf")) [341..345]
        "#)
    }
}
