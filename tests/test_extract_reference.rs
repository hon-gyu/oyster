use insta::{assert_debug_snapshot, assert_snapshot};
use markdown_tools::ast::Tree;
use markdown_tools::link::{Reference, Referenceable, scan_note, scan_vault};
use std::fs;
use std::path::PathBuf;

#[test]
fn test_scan_vault() {
    let dir = PathBuf::from("tests/data/vaults/tt");
    let root_dir = PathBuf::from("tests/data/vaults/tt");
    let (_, referenceables, references) = scan_vault(&dir, &root_dir);
    assert_debug_snapshot!(references, @r########"
    [
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 167..223,
            dest: "Three laws of motion",
            display_text: "Three laws of motion 11",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 254..289,
            dest: "#Level 3 title",
            display_text: "Level 3 title",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 356..395,
            dest: "Note 2#Some level 2 title",
            display_text: "22",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 514..521,
            dest: "()",
            display_text: "www",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 590..596,
            dest: "ww",
            display_text: "",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 647..651,
            dest: "()",
            display_text: "",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 700..733,
            dest: "Three laws of motion",
            display_text: "",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 951..974,
            dest: "Three laws of motion",
            display_text: "Three laws of motion",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1015..1041,
            dest: "Three laws of motion.md",
            display_text: "Three laws of motion.md",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1075..1095,
            dest: "Note 2",
            display_text: " Note two",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1125..1142,
            dest: "#Level 3 title",
            display_text: "#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1203..1220,
            dest: "#Level 4 title",
            display_text: "#Level 4 title",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1282..1292,
            dest: "#random",
            display_text: "#random",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1358..1386,
            dest: "Note 2#Some level 2 title",
            display_text: "Note 2#Some level 2 title",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1457..1499,
            dest: "Note 2#Some level 2 title#Level 3 title",
            display_text: "Note 2#Some level 2 title#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1536..1566,
            dest: "Note 2#random#Level 3 title",
            display_text: "Note 2#random#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1645..1668,
            dest: "Note 2#Level 3 title",
            display_text: "Note 2#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1697..1709,
            dest: "Note 2#L4",
            display_text: "Note 2#L4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1745..1776,
            dest: "Note 2#Some level 2 title#L4",
            display_text: "Note 2#Some level 2 title#L4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 1942..1964,
            dest: "Non-existing note 4",
            display_text: "Non-existing note 4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2040..2044,
            dest: "#",
            display_text: "#",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2094..2105,
            dest: "Note 2##",
            display_text: "Note 2##",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2186..2210,
            dest: "#######Link to figure",
            display_text: "#######Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2243..2266,
            dest: "######Link to figure",
            display_text: "######Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2297..2318,
            dest: "####Link to figure",
            display_text: "####Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2348..2368,
            dest: "###Link to figure",
            display_text: "###Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2396..2414,
            dest: "#Link to figure",
            display_text: "#Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2447..2459,
            dest: "#L2",
            display_text: " #L4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2554..2571,
            dest: "Note 2",
            display_text: " 2 | 3",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2672..2683,
            dest: "###L2#L4",
            display_text: "###L2#L4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2743..2758,
            dest: "##L2######L4",
            display_text: "##L2######L4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2817..2831,
            dest: "##L2#####L4",
            display_text: "##L2#####L4",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2893..2910,
            dest: "##L2#####L4#L3",
            display_text: "##L2#####L4#L3",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 2966..2991,
            dest: "##L2#####L4#Another L3",
            display_text: "##L2#####L4#Another L3",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 3332..3349,
            dest: "##L2######L4",
            display_text: "1",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 3408..3424,
            dest: "##L2#####L4",
            display_text: "2",
        },
        Reference {
            kind: MarkdownLink,
            path: "Note 1.md",
            range: 3486..3505,
            dest: "##L2#####L4#L3",
            display_text: "3",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 3577..3591,
            dest: "Figure1.jpg",
            display_text: "Figure1.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 3700..3716,
            dest: "Figure1.jpg#2",
            display_text: "Figure1.jpg#2",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 3762..3780,
            dest: "Figure1.jpg",
            display_text: " 2",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 3867..3884,
            dest: "Figure1.jpg.md",
            display_text: "Figure1.jpg.md",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 3975..3995,
            dest: "Figure1.jpg.md.md",
            display_text: "Figure1.jpg.md.md",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4020..4036,
            dest: "Figure1#2.jpg",
            display_text: "Figure1#2.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4159..4175,
            dest: "Figure1",
            display_text: "2.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4298..4314,
            dest: "Figure1^2.jpg",
            display_text: "Figure1^2.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4424..4431,
            dest: "dir/",
            display_text: "dir/",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4861..4895,
            dest: "dir/inner_dir/note_in_inner_dir",
            display_text: "dir/inner_dir/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4935..4965,
            dest: "inner_dir/note_in_inner_dir",
            display_text: "inner_dir/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 4999..5023,
            dest: "dir/note_in_inner_dir",
            display_text: "dir/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5095..5122,
            dest: "random/note_in_inner_dir",
            display_text: "random/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5263..5278,
            dest: "inner_dir/hi",
            display_text: "inner_dir/hi",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5309..5331,
            dest: "dir/indir_same_name",
            display_text: "dir/indir_same_name",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5358..5376,
            dest: "indir_same_name",
            display_text: "indir_same_name",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5446..5455,
            dest: "indir2",
            display_text: "indir2",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5502..5514,
            dest: "Something",
            display_text: "Something",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5631..5659,
            dest: "unsupported_text_file.txt",
            display_text: "unsupported_text_file.txt",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5740..5757,
            dest: "a.joiwduvqneoi",
            display_text: "a.joiwduvqneoi",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5793..5802,
            dest: "Note 1",
            display_text: "Note 1",
        },
        Reference {
            kind: Embed,
            path: "Note 1.md",
            range: 5896..5911,
            dest: "Figure1.jpg",
            display_text: "Figure1.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "Note 1.md",
            range: 5936..5954,
            dest: "empty_video.mp4",
            display_text: "empty_video.mp4",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 251..265,
            dest: "#^quotation",
            display_text: "#^quotation",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 313..325,
            dest: "#^callout",
            display_text: "#^callout",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 357..371,
            dest: "#^paragraph",
            display_text: "#^paragraph",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 396..412,
            dest: "#^p-with-code",
            display_text: "#^p-with-code",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 437..452,
            dest: "#^paragraph2",
            display_text: "#^paragraph2",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 544..554,
            dest: "#^table",
            display_text: "#^table",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 632..646,
            dest: "#^firstline",
            display_text: "#^firstline",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 678..692,
            dest: "#^inneritem",
            display_text: "#^inneritem",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 912..925,
            dest: "#^tableref",
            display_text: "#^tableref",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1082..1096,
            dest: "#^tableref3",
            display_text: "#^tableref3",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1100..1114,
            dest: "#^tableref2",
            display_text: "#^tableref2",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1256..1266,
            dest: "#^works",
            display_text: "#^works",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1686..1700,
            dest: "#^firstline",
            display_text: "#^firstline",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1732..1746,
            dest: "#^inneritem",
            display_text: "#^inneritem",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1864..1879,
            dest: "#^firstline1",
            display_text: "#^firstline1",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1932..1947,
            dest: "#^inneritem1",
            display_text: "#^inneritem1",
        },
        Reference {
            kind: WikiLink,
            path: "block.md",
            range: 1999..2012,
            dest: "#^fulllst1",
            display_text: "#^fulllst1",
        },
    ]
    "########);
    assert_debug_snapshot!(referenceables, @r#"
    [
        Note {
            path: "Note 1.md",
            children: [
                Heading {
                    path: "Note 1.md",
                    level: H3,
                    text: "Level 3 title",
                    range: 55..73,
                },
                Heading {
                    path: "Note 1.md",
                    level: H4,
                    text: "Level 4 title",
                    range: 73..92,
                },
                Heading {
                    path: "Note 1.md",
                    level: H3,
                    text: "Example (level 3)",
                    range: 93..115,
                },
                Heading {
                    path: "Note 1.md",
                    level: H6,
                    text: "Markdown link: [x](y)",
                    range: 116..147,
                },
                Heading {
                    path: "Note 1.md",
                    level: H6,
                    text: "Wiki link: [[x#]] | [[x#^block_identifier]]",
                    range: 887..942,
                },
                Heading {
                    path: "Note 1.md",
                    level: H5,
                    text: "Link to asset",
                    range: 3536..3556,
                },
                Heading {
                    path: "Note 1.md",
                    level: H2,
                    text: "L2",
                    range: 5957..5963,
                },
                Heading {
                    path: "Note 1.md",
                    level: H3,
                    text: "L3",
                    range: 5964..5971,
                },
                Heading {
                    path: "Note 1.md",
                    level: H4,
                    text: "L4",
                    range: 5971..5979,
                },
                Heading {
                    path: "Note 1.md",
                    level: H3,
                    text: "Another L3",
                    range: 5979..5994,
                },
                Heading {
                    path: "Note 1.md",
                    level: H2,
                    text: "",
                    range: 5999..6003,
                },
            ],
        },
        Asset {
            path: "a.joiwduvqneoi",
        },
        Note {
            path: "Figure1.jpg.md",
            children: [],
        },
        Asset {
            path: "Something",
        },
        Note {
            path: "Three laws of motion.md",
            children: [],
        },
        Note {
            path: "indir_same_name.md",
            children: [],
        },
        Note {
            path: "ww.md",
            children: [],
        },
        Note {
            path: "unsupported_text_file.txt.md",
            children: [],
        },
        Note {
            path: "Figure1.jpg.md.md",
            children: [],
        },
        Note {
            path: "().md",
            children: [],
        },
        Note {
            path: "block.md",
            children: [
                Block {
                    path: "block.md",
                    identifier: "paragraph",
                    kind: InlineParagraph,
                    range: 0..23,
                },
                Block {
                    path: "block.md",
                    identifier: "p-with-code",
                    kind: InlineParagraph,
                    range: 24..66,
                },
                Block {
                    path: "block.md",
                    identifier: "paragraph2",
                    kind: Paragraph,
                    range: 67..79,
                },
                Block {
                    path: "block.md",
                    identifier: "fulllist",
                    kind: List,
                    range: 93..126,
                },
                Block {
                    path: "block.md",
                    identifier: "table",
                    kind: Table,
                    range: 137..217,
                },
                Block {
                    path: "block.md",
                    identifier: "quotation",
                    kind: BlockQuote,
                    range: 226..238,
                },
                Block {
                    path: "block.md",
                    identifier: "callout",
                    kind: BlockQuote,
                    range: 269..302,
                },
                Block {
                    path: "block.md",
                    identifier: "firstline",
                    kind: InlineListItem,
                    range: 563..613,
                },
                Block {
                    path: "block.md",
                    identifier: "inneritem",
                    kind: InlineListItem,
                    range: 589..613,
                },
                Heading {
                    path: "block.md",
                    level: H6,
                    text: "Edge case: a later block identifier invalidate previous one",
                    range: 733..801,
                },
                Block {
                    path: "block.md",
                    identifier: "tableref",
                    kind: Table,
                    range: 802..882,
                },
                Block {
                    path: "block.md",
                    identifier: "tableref2",
                    kind: Table,
                    range: 929..1009,
                },
                Block {
                    path: "block.md",
                    identifier: "tableref3",
                    kind: Paragraph,
                    range: 1010..1021,
                },
                Heading {
                    path: "block.md",
                    level: H6,
                    text: "Edge case: the number of blank lines before identifier doesn’t matter",
                    range: 1164..1242,
                },
                Block {
                    path: "block.md",
                    identifier: "works",
                    kind: InlineParagraph,
                    range: 1243..1255,
                },
                Heading {
                    path: "block.md",
                    level: H6,
                    text: "Edge case: full reference to a list make its inner state not refereceable",
                    range: 1534..1616,
                },
                Block {
                    path: "block.md",
                    identifier: "firstline",
                    kind: InlineParagraph,
                    range: 1619..1644,
                },
                Block {
                    path: "block.md",
                    identifier: "inneritem",
                    kind: InlineListItem,
                    range: 1643..1667,
                },
                Block {
                    path: "block.md",
                    identifier: "firstline1",
                    kind: InlineParagraph,
                    range: 1783..1809,
                },
                Block {
                    path: "block.md",
                    identifier: "fulllist1",
                    kind: InlineListItem,
                    range: 1808..1845,
                },
                Heading {
                    path: "block.md",
                    level: H6,
                    text: "Edge case: When there are more than one identical identifiers",
                    range: 2041..2111,
                },
            ],
        },
        Asset {
            path: "Figure1#2.jpg",
        },
        Asset {
            path: "Note 1",
        },
        Note {
            path: "dir.md",
            children: [],
        },
        Note {
            path: "a.joiwduvqneoi.md",
            children: [],
        },
        Asset {
            path: "empty_video.mp4",
        },
        Note {
            path: "Hi.txt.md",
            children: [],
        },
        Note {
            path: "dir/inner_dir/note_in_inner_dir.md",
            children: [],
        },
        Note {
            path: "dir/indir_same_name.md",
            children: [],
        },
        Note {
            path: "dir/indir2.md",
            children: [],
        },
        Asset {
            path: "unsupported_text_file.txt",
        },
        Asset {
            path: "unsupported.unsupported",
        },
        Note {
            path: "Figure1.md",
            children: [],
        },
        Asset {
            path: "Figure1^2.jpg",
        },
        Asset {
            path: "Figure1.jpg",
        },
        Note {
            path: "Note 2.md",
            children: [
                Heading {
                    path: "Note 2.md",
                    level: H2,
                    text: "Some level 2 title",
                    range: 1..23,
                },
                Heading {
                    path: "Note 2.md",
                    level: H4,
                    text: "L4",
                    range: 24..32,
                },
                Heading {
                    path: "Note 2.md",
                    level: H3,
                    text: "Level 3 title",
                    range: 33..51,
                },
                Heading {
                    path: "Note 2.md",
                    level: H2,
                    text: "Another level 2 title",
                    range: 53..77,
                },
            ],
        },
        Asset {
            path: "Figure1|2.jpg",
        },
    ]
    "#);
}

#[test]
fn test_exract_references_and_referenceables() {
    let path = PathBuf::from("tests/data/vaults/tt/Note 1.md");
    let (_, references, referenceables): (
        _,
        Vec<Reference>,
        Vec<Referenceable>,
    ) = scan_note(&path);
    assert_debug_snapshot!(references, @r########"
    [
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 167..223,
            dest: "Three laws of motion",
            display_text: "Three laws of motion 11",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 254..289,
            dest: "#Level 3 title",
            display_text: "Level 3 title",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 356..395,
            dest: "Note 2#Some level 2 title",
            display_text: "22",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 514..521,
            dest: "()",
            display_text: "www",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 590..596,
            dest: "ww",
            display_text: "",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 647..651,
            dest: "()",
            display_text: "",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 700..733,
            dest: "Three laws of motion",
            display_text: "",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 951..974,
            dest: "Three laws of motion",
            display_text: "Three laws of motion",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1015..1041,
            dest: "Three laws of motion.md",
            display_text: "Three laws of motion.md",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1075..1095,
            dest: "Note 2",
            display_text: " Note two",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1125..1142,
            dest: "#Level 3 title",
            display_text: "#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1203..1220,
            dest: "#Level 4 title",
            display_text: "#Level 4 title",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1282..1292,
            dest: "#random",
            display_text: "#random",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1358..1386,
            dest: "Note 2#Some level 2 title",
            display_text: "Note 2#Some level 2 title",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1457..1499,
            dest: "Note 2#Some level 2 title#Level 3 title",
            display_text: "Note 2#Some level 2 title#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1536..1566,
            dest: "Note 2#random#Level 3 title",
            display_text: "Note 2#random#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1645..1668,
            dest: "Note 2#Level 3 title",
            display_text: "Note 2#Level 3 title",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1697..1709,
            dest: "Note 2#L4",
            display_text: "Note 2#L4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1745..1776,
            dest: "Note 2#Some level 2 title#L4",
            display_text: "Note 2#Some level 2 title#L4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 1942..1964,
            dest: "Non-existing note 4",
            display_text: "Non-existing note 4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2040..2044,
            dest: "#",
            display_text: "#",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2094..2105,
            dest: "Note 2##",
            display_text: "Note 2##",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2186..2210,
            dest: "#######Link to figure",
            display_text: "#######Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2243..2266,
            dest: "######Link to figure",
            display_text: "######Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2297..2318,
            dest: "####Link to figure",
            display_text: "####Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2348..2368,
            dest: "###Link to figure",
            display_text: "###Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2396..2414,
            dest: "#Link to figure",
            display_text: "#Link to figure",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2447..2459,
            dest: "#L2",
            display_text: " #L4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2554..2571,
            dest: "Note 2",
            display_text: " 2 | 3",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2672..2683,
            dest: "###L2#L4",
            display_text: "###L2#L4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2743..2758,
            dest: "##L2######L4",
            display_text: "##L2######L4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2817..2831,
            dest: "##L2#####L4",
            display_text: "##L2#####L4",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2893..2910,
            dest: "##L2#####L4#L3",
            display_text: "##L2#####L4#L3",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 2966..2991,
            dest: "##L2#####L4#Another L3",
            display_text: "##L2#####L4#Another L3",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3332..3349,
            dest: "##L2######L4",
            display_text: "1",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3408..3424,
            dest: "##L2#####L4",
            display_text: "2",
        },
        Reference {
            kind: MarkdownLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3486..3505,
            dest: "##L2#####L4#L3",
            display_text: "3",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3577..3591,
            dest: "Figure1.jpg",
            display_text: "Figure1.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3700..3716,
            dest: "Figure1.jpg#2",
            display_text: "Figure1.jpg#2",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3762..3780,
            dest: "Figure1.jpg",
            display_text: " 2",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3867..3884,
            dest: "Figure1.jpg.md",
            display_text: "Figure1.jpg.md",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 3975..3995,
            dest: "Figure1.jpg.md.md",
            display_text: "Figure1.jpg.md.md",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4020..4036,
            dest: "Figure1#2.jpg",
            display_text: "Figure1#2.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4159..4175,
            dest: "Figure1",
            display_text: "2.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4298..4314,
            dest: "Figure1^2.jpg",
            display_text: "Figure1^2.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4424..4431,
            dest: "dir/",
            display_text: "dir/",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4861..4895,
            dest: "dir/inner_dir/note_in_inner_dir",
            display_text: "dir/inner_dir/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4935..4965,
            dest: "inner_dir/note_in_inner_dir",
            display_text: "inner_dir/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 4999..5023,
            dest: "dir/note_in_inner_dir",
            display_text: "dir/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5095..5122,
            dest: "random/note_in_inner_dir",
            display_text: "random/note_in_inner_dir",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5263..5278,
            dest: "inner_dir/hi",
            display_text: "inner_dir/hi",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5309..5331,
            dest: "dir/indir_same_name",
            display_text: "dir/indir_same_name",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5358..5376,
            dest: "indir_same_name",
            display_text: "indir_same_name",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5446..5455,
            dest: "indir2",
            display_text: "indir2",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5502..5514,
            dest: "Something",
            display_text: "Something",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5631..5659,
            dest: "unsupported_text_file.txt",
            display_text: "unsupported_text_file.txt",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5740..5757,
            dest: "a.joiwduvqneoi",
            display_text: "a.joiwduvqneoi",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5793..5802,
            dest: "Note 1",
            display_text: "Note 1",
        },
        Reference {
            kind: Embed,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5896..5911,
            dest: "Figure1.jpg",
            display_text: "Figure1.jpg",
        },
        Reference {
            kind: WikiLink,
            path: "tests/data/vaults/tt/Note 1.md",
            range: 5936..5954,
            dest: "empty_video.mp4",
            display_text: "empty_video.mp4",
        },
    ]
    "########);
    assert_debug_snapshot!(referenceables, @r#"
            [
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H3,
                    text: "Level 3 title",
                    range: 55..73,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H4,
                    text: "Level 4 title",
                    range: 73..92,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H3,
                    text: "Example (level 3)",
                    range: 93..115,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H6,
                    text: "Markdown link: [x](y)",
                    range: 116..147,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H6,
                    text: "Wiki link: [[x#]] | [[x#^block_identifier]]",
                    range: 887..942,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H5,
                    text: "Link to asset",
                    range: 3536..3556,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H2,
                    text: "L2",
                    range: 5957..5963,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H3,
                    text: "L3",
                    range: 5964..5971,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H4,
                    text: "L4",
                    range: 5971..5979,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H3,
                    text: "Another L3",
                    range: 5979..5994,
                },
                Heading {
                    path: "tests/data/vaults/tt/Note 1.md",
                    level: H2,
                    text: "",
                    range: 5999..6003,
                },
            ]
            "#);
}

#[test]
fn test_parse_ast_with_links() {
    let path = "tests/data/vaults/tt/Note 1.md";
    let text = fs::read_to_string(path).unwrap();
    let tree = Tree::new(&text);
    assert_snapshot!(tree.root_node, @r########"
            Document [0..6003]
              List(None) [0..55]
                Item [0..55]
                  Text(Borrowed("Note in Obsidian cannot have # ^ ")) [2..35]
                  Text(Borrowed("[")) [35..36]
                  Text(Borrowed(" ")) [36..37]
                  Text(Borrowed("]")) [37..38]
                  Text(Borrowed(" | in the title.")) [38..54]
              Heading { level: H3, id: None, classes: [], attrs: [] } [55..73]
                Text(Borrowed("Level 3 title")) [59..72]
              Heading { level: H4, id: None, classes: [], attrs: [] } [73..92]
                Text(Borrowed("Level 4 title")) [78..91]
              Heading { level: H3, id: None, classes: [], attrs: [] } [93..115]
                Text(Borrowed("Example (level 3)")) [97..114]
              Heading { level: H6, id: None, classes: [], attrs: [] } [116..147]
                Text(Borrowed("Markdown link: ")) [123..138]
                Code(Borrowed("[x](y)")) [138..146]
              List(None) [147..887]
                Item [147..224]
                  Text(Borrowed("percent encoding: ")) [149..167]
                  Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [167..223]
                    Text(Borrowed("Three laws of motion 11")) [168..191]
                Item [224..331]
                  Text(Borrowed("heading  in the same file:  ")) [226..254]
                  Link { link_type: Inline, dest_url: Borrowed("#Level%203%20title"), title: Borrowed(""), id: Borrowed("") } [254..289]
                    Text(Borrowed("Level 3 title")) [255..268]
                  List(None) [289..331]
                    Item [289..331]
                      Code(Borrowed("[Level 3 title](#Level%203%20title)")) [293..330]
                Item [331..499]
                  Text(Borrowed("different file heading ")) [333..356]
                  Link { link_type: Inline, dest_url: Borrowed("Note%202#Some%20level%202%20title"), title: Borrowed(""), id: Borrowed("") } [356..395]
                    Text(Borrowed("22")) [357..359]
                  List(None) [395..499]
                    Item [395..441]
                      Code(Borrowed("[22](Note%202#Some%20level%202%20title)")) [399..440]
                    Item [440..499]
                      Text(Borrowed("the heading is level 2 but we don")) [444..477]
                      Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [477..478]
                      Text(Borrowed("t need to specify it")) [478..498]
                Item [499..575]
                  Text(Borrowed("empty link 1 ")) [501..514]
                  Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [514..521]
                    Text(Borrowed("www")) [515..518]
                  List(None) [521..575]
                    Item [521..575]
                      Text(Borrowed("empty markdown link ")) [525..545]
                      Code(Borrowed("[]()")) [545..551]
                      Text(Borrowed(" points to note ")) [551..567]
                      Code(Borrowed("().md")) [567..574]
                Item [575..632]
                  Text(Borrowed("empty link 2 ")) [577..590]
                  Link { link_type: Inline, dest_url: Borrowed("ww"), title: Borrowed(""), id: Borrowed("") } [590..596]
                  List(None) [596..632]
                    Item [596..609]
                      Code(Borrowed("[](ww)")) [600..608]
                    Item [608..632]
                      Text(Borrowed("points to note ")) [612..627]
                      Code(Borrowed("ww")) [627..631]
                Item [632..685]
                  Text(Borrowed("empty link 3 ")) [634..647]
                  Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [647..651]
                  List(None) [651..685]
                    Item [651..662]
                      Code(Borrowed("[]()")) [655..661]
                    Item [661..685]
                      Text(Borrowed("points to note ")) [665..680]
                      Code(Borrowed("()")) [680..684]
                Item [685..887]
                  Text(Borrowed("empty link 4 ")) [687..700]
                  Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [700..733]
                  List(None) [733..887]
                    Item [733..773]
                      Code(Borrowed("[](Three%20laws%20of%20motion.md)")) [737..772]
                    Item [772..814]
                      Text(Borrowed("points to note ")) [776..791]
                      Code(Borrowed("Three laws of motion")) [791..813]
                    Item [813..887]
                      Text(Borrowed("the first part of markdown link is displayed text and doesn")) [817..876]
                      Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [876..877]
                      Text(Borrowed("t matter")) [877..885]
              Heading { level: H6, id: None, classes: [], attrs: [] } [887..942]
                Text(Borrowed("Wiki link: ")) [894..905]
                Code(Borrowed("[[x#]]")) [905..913]
                Text(Borrowed(" | ")) [913..916]
                Code(Borrowed("[[x#^block_identifier]]")) [916..941]
              List(None) [942..3536]
                Item [942..976]
                  Text(Borrowed("basic: ")) [944..951]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion"), title: Borrowed(""), id: Borrowed("") } [951..974]
                    Text(Borrowed("Three laws of motion")) [953..973]
                Item [976..1043]
                  Text(Borrowed("explicit markdown extension in name: ")) [978..1015]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion.md"), title: Borrowed(""), id: Borrowed("") } [1015..1041]
                    Text(Borrowed("Three laws of motion.md")) [1017..1040]
                Item [1043..1097]
                  Text(Borrowed("with pipe for displayed text: ")) [1045..1075]
                  Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [1075..1095]
                    Text(Borrowed(" Note two")) [1085..1094]
                Item [1097..1168]
                  Text(Borrowed("heading in the same note: ")) [1099..1125]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1125..1142]
                    Text(Borrowed("#Level 3 title")) [1127..1141]
                  List(None) [1143..1168]
                    Item [1143..1168]
                      Code(Borrowed("[[#Level 3 title]]")) [1147..1167]
                Item [1168..1246]
                  Text(Borrowed("nested heading in the same note: ")) [1170..1203]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 4 title"), title: Borrowed(""), id: Borrowed("") } [1203..1220]
                    Text(Borrowed("#Level 4 title")) [1205..1219]
                  List(None) [1221..1246]
                    Item [1221..1246]
                      Code(Borrowed("[[#Level 4 title]]")) [1225..1245]
                Item [1246..1331]
                  Text(Borrowed("invalid heading in the same note: ")) [1248..1282]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#random"), title: Borrowed(""), id: Borrowed("") } [1282..1292]
                    Text(Borrowed("#random")) [1284..1291]
                  List(None) [1293..1331]
                    Item [1293..1311]
                      Code(Borrowed("[[#random]]")) [1297..1310]
                    Item [1310..1331]
                      Text(Borrowed("fallback to note")) [1314..1330]
                Item [1331..1423]
                  Text(Borrowed("heading in another note: ")) [1333..1358]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title"), title: Borrowed(""), id: Borrowed("") } [1358..1386]
                    Text(Borrowed("Note 2#Some level 2 title")) [1360..1385]
                  List(None) [1387..1423]
                    Item [1387..1423]
                      Code(Borrowed("[[Note 2#Some level 2 title]]")) [1391..1422]
                Item [1423..1501]
                  Text(Borrowed("nested heading in another note: ")) [1425..1457]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1457..1499]
                    Text(Borrowed("Note 2#Some level 2 title#Level 3 title")) [1459..1498]
                Item [1501..1618]
                  Text(Borrowed("invalid heading in another note: ")) [1503..1536]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#random#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1536..1566]
                    Text(Borrowed("Note 2#random#Level 3 title")) [1538..1565]
                  List(None) [1567..1618]
                    Item [1567..1618]
                      Text(Borrowed("fallback to note if the heading doesn")) [1571..1608]
                      Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1608..1609]
                      Text(Borrowed("t exist")) [1609..1616]
                Item [1618..1670]
                  Text(Borrowed("heading in another note: ")) [1620..1645]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1645..1668]
                    Text(Borrowed("Note 2#Level 3 title")) [1647..1667]
                Item [1670..1711]
                  Text(Borrowed("heading in another note: ")) [1672..1697]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#L4"), title: Borrowed(""), id: Borrowed("") } [1697..1709]
                    Text(Borrowed("Note 2#L4")) [1699..1708]
                Item [1711..1921]
                  Text(Borrowed("nested heading in another note: ")) [1713..1745]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#L4"), title: Borrowed(""), id: Borrowed("") } [1745..1776]
                    Text(Borrowed("Note 2#Some level 2 title#L4")) [1747..1775]
                  List(None) [1777..1921]
                    Item [1777..1850]
                      Text(Borrowed("when there")) [1781..1791]
                      Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1791..1792]
                      Text(Borrowed("s multiple levels, the level doesn")) [1792..1826]
                      Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1826..1827]
                      Text(Borrowed("t need to be specified")) [1827..1849]
                    Item [1849..1921]
                      Text(Borrowed("it will match as long as the ancestor-descendant relationship holds")) [1853..1920]
                Item [1921..1966]
                  Text(Borrowed("non-existing note: ")) [1923..1942]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Non-existing note 4"), title: Borrowed(""), id: Borrowed("") } [1942..1964]
                    Text(Borrowed("Non-existing note 4")) [1944..1963]
                Item [1966..2011]
                  Text(Borrowed("empty link: ")) [1968..1980]
                  Text(Borrowed("[")) [1980..1981]
                  Text(Borrowed("[")) [1981..1982]
                  Text(Borrowed("]")) [1982..1983]
                  Text(Borrowed("]")) [1983..1984]
                  List(None) [1984..2011]
                    Item [1984..2011]
                      Text(Borrowed("points to current note")) [1988..2010]
                Item [2011..2128]
                  Text(Borrowed("empty heading:")) [2013..2027]
                  List(None) [2027..2128]
                    Item [2027..2074]
                      Code(Borrowed("[[#]]")) [2031..2038]
                      Text(Borrowed(": ")) [2038..2040]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#"), title: Borrowed(""), id: Borrowed("") } [2040..2044]
                        Text(Borrowed("#")) [2042..2043]
                      List(None) [2047..2074]
                        Item [2047..2074]
                          Text(Borrowed("points to current note")) [2051..2073]
                    Item [2073..2128]
                      Code(Borrowed("[[Note 2##]]")) [2077..2091]
                      Text(Borrowed(":  ")) [2091..2094]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2##"), title: Borrowed(""), id: Borrowed("") } [2094..2105]
                        Text(Borrowed("Note 2##")) [2096..2104]
                      List(None) [2107..2128]
                        Item [2107..2128]
                          Text(Borrowed("points to Note 2")) [2111..2127]
                Item [2128..2416]
                  Text(Borrowed("incorrect heading level")) [2130..2153]
                  List(None) [2153..2416]
                    Item [2153..2212]
                      Code(Borrowed("[[#######Link to figure]]")) [2157..2184]
                      Text(Borrowed(": ")) [2184..2186]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2186..2210]
                        Text(Borrowed("#######Link to figure")) [2188..2209]
                    Item [2211..2268]
                      Code(Borrowed("[[######Link to figure]]")) [2215..2241]
                      Text(Borrowed(": ")) [2241..2243]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2243..2266]
                        Text(Borrowed("######Link to figure")) [2245..2265]
                    Item [2267..2320]
                      Code(Borrowed("[[####Link to figure]]")) [2271..2295]
                      Text(Borrowed(": ")) [2295..2297]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("####Link to figure"), title: Borrowed(""), id: Borrowed("") } [2297..2318]
                        Text(Borrowed("####Link to figure")) [2299..2317]
                    Item [2319..2370]
                      Code(Borrowed("[[###Link to figure]]")) [2323..2346]
                      Text(Borrowed(": ")) [2346..2348]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###Link to figure"), title: Borrowed(""), id: Borrowed("") } [2348..2368]
                        Text(Borrowed("###Link to figure")) [2350..2367]
                    Item [2369..2416]
                      Code(Borrowed("[[#Link to figure]]")) [2373..2394]
                      Text(Borrowed(": ")) [2394..2396]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Link to figure"), title: Borrowed(""), id: Borrowed("") } [2396..2414]
                        Text(Borrowed("#Link to figure")) [2398..2413]
                Item [2416..2536]
                  Text(Borrowed("ambiguous pipe and heading: ")) [2419..2447]
                  Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("#L2 "), title: Borrowed(""), id: Borrowed("") } [2447..2459]
                    Text(Borrowed(" #L4")) [2454..2458]
                  List(None) [2461..2536]
                    Item [2461..2481]
                      Code(Borrowed("[[#L2 | #L4]]")) [2465..2480]
                    Item [2481..2498]
                      Text(Borrowed("points to L2")) [2485..2497]
                    Item [2498..2536]
                      Text(Borrowed("things after the pipe is escaped")) [2502..2534]
                Item [2536..2624]
                  Text(Borrowed("multiple pipe: ")) [2539..2554]
                  Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [2554..2571]
                    Text(Borrowed(" 2 | 3")) [2564..2570]
                  List(None) [2573..2624]
                    Item [2573..2598]
                      Code(Borrowed("[[Note 2 | 2 | 3]]")) [2577..2597]
                    Item [2598..2624]
                      Text(Borrowed("this points to Note 2")) [2602..2623]
                Item [2624..3117]
                  Text(Borrowed("incorrect nested heading")) [2626..2650]
                  List(None) [2651..3117]
                    Item [2651..2720]
                      Code(Borrowed("[[###L2#L4]]")) [2655..2669]
                      Text(Borrowed(":  ")) [2669..2672]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###L2#L4"), title: Borrowed(""), id: Borrowed("") } [2672..2683]
                        Text(Borrowed("###L2#L4")) [2674..2682]
                      List(None) [2685..2720]
                        Item [2685..2720]
                          Text(Borrowed("points to L4 heading correctly")) [2689..2719]
                    Item [2719..2795]
                      Code(Borrowed("[[##L2######L4]]")) [2723..2741]
                      Text(Borrowed(": ")) [2741..2743]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [2743..2758]
                        Text(Borrowed("##L2######L4")) [2745..2757]
                      List(None) [2760..2795]
                        Item [2760..2795]
                          Text(Borrowed("points to L4 heading correctly")) [2764..2794]
                    Item [2794..2868]
                      Code(Borrowed("[[##L2#####L4]]")) [2798..2815]
                      Text(Borrowed(": ")) [2815..2817]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [2817..2831]
                        Text(Borrowed("##L2#####L4")) [2819..2830]
                      List(None) [2833..2868]
                        Item [2833..2868]
                          Text(Borrowed("points to L4 heading correctly")) [2837..2867]
                    Item [2867..2941]
                      Code(Borrowed("[[##L2#####L4#L3]]")) [2871..2891]
                      Text(Borrowed(": ")) [2891..2893]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [2893..2910]
                        Text(Borrowed("##L2#####L4#L3")) [2895..2909]
                      List(None) [2912..2941]
                        Item [2912..2941]
                          Text(Borrowed("fallback to current note")) [2916..2940]
                    Item [2940..3022]
                      Code(Borrowed("[[##L2#####L4#L3]]")) [2944..2964]
                      Text(Borrowed(": ")) [2964..2966]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#Another L3"), title: Borrowed(""), id: Borrowed("") } [2966..2991]
                        Text(Borrowed("##L2#####L4#Another L3")) [2968..2990]
                      List(None) [2993..3022]
                        Item [2993..3022]
                          Text(Borrowed("fallback to current note")) [2997..3021]
                    Item [3021..3117]
                      Text(Borrowed("for displayed text, the first hash is removed, the subsequent nesting ones are not affected")) [3025..3116]
                Item [3117..3237]
                  Text(Borrowed("↳ it looks like whenever there")) [3119..3151]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3151..3152]
                  Text(Borrowed("s multiple hash, it")) [3152..3171]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3171..3172]
                  Text(Borrowed("s all stripped. only the ancestor-descendant relationship matter")) [3172..3236]
                Item [3237..3536]
                  Text(Borrowed("I don")) [3239..3244]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3244..3245]
                  Text(Borrowed("t think there")) [3245..3258]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3258..3259]
                  Text(Borrowed("s a different between Wikilink and Markdown link")) [3259..3307]
                  List(None) [3307..3536]
                    Item [3307..3385]
                      Code(Borrowed("[1](##L2######L4)")) [3311..3330]
                      Text(Borrowed(": ")) [3330..3332]
                      Link { link_type: Inline, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [3332..3349]
                        Text(Borrowed("1")) [3333..3334]
                      List(None) [3350..3385]
                        Item [3350..3385]
                          Text(Borrowed("points to L4 heading correctly")) [3354..3384]
                    Item [3384..3460]
                      Code(Borrowed("[2](##L2#####L4)")) [3388..3406]
                      Text(Borrowed(": ")) [3406..3408]
                      Link { link_type: Inline, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [3408..3424]
                        Text(Borrowed("2")) [3409..3410]
                      List(None) [3425..3460]
                        Item [3425..3460]
                          Text(Borrowed("points to L4 heading correctly")) [3429..3459]
                    Item [3459..3536]
                      Code(Borrowed("[3](##L2#####L4#L3)")) [3463..3484]
                      Text(Borrowed(": ")) [3484..3486]
                      Link { link_type: Inline, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [3486..3505]
                        Text(Borrowed("3")) [3487..3488]
                      List(None) [3506..3536]
                        Item [3506..3536]
                          Text(Borrowed("fallback to current note")) [3510..3534]
              Heading { level: H5, id: None, classes: [], attrs: [] } [3536..3556]
                Text(Borrowed("Link to asset")) [3542..3555]
              List(None) [3556..5876]
                Item [3556..3677]
                  Code(Borrowed("[[Figure1.jpg]]")) [3558..3575]
                  Text(Borrowed(": ")) [3575..3577]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg"), title: Borrowed(""), id: Borrowed("") } [3577..3591]
                    Text(Borrowed("Figure1.jpg")) [3579..3590]
                  List(None) [3592..3677]
                    Item [3592..3677]
                      Text(Borrowed("even if there exists a note called ")) [3596..3631]
                      Code(Borrowed("Figure1.jpg")) [3631..3644]
                      Text(Borrowed(", the asset will take precedence")) [3644..3676]
                Item [3677..3737]
                  Code(Borrowed("[[Figure1.jpg#2]]")) [3679..3698]
                  Text(Borrowed(": ")) [3698..3700]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg#2"), title: Borrowed(""), id: Borrowed("") } [3700..3716]
                    Text(Borrowed("Figure1.jpg#2")) [3702..3715]
                  List(None) [3717..3737]
                    Item [3717..3737]
                      Text(Borrowed("points to image")) [3721..3736]
                Item [3737..3843]
                  Code(Borrowed("[[Figure1.jpg | 2]]")) [3739..3760]
                  Text(Borrowed(": ")) [3760..3762]
                  Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Figure1.jpg "), title: Borrowed(""), id: Borrowed("") } [3762..3780]
                    Text(Borrowed(" 2")) [3777..3779]
                  List(None) [3781..3843]
                    Item [3781..3801]
                      Text(Borrowed("points to image")) [3785..3800]
                    Item [3800..3843]
                      Text(Borrowed("leading and ending spaces are stripped")) [3804..3842]
                Item [3843..3948]
                  Code(Borrowed("[[Figure1.jpg.md]]")) [3845..3865]
                  Text(Borrowed(": ")) [3865..3867]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg.md"), title: Borrowed(""), id: Borrowed("") } [3867..3884]
                    Text(Borrowed("Figure1.jpg.md")) [3869..3883]
                  List(None) [3885..3948]
                    Item [3885..3948]
                      Text(Borrowed("with explicit ")) [3889..3903]
                      Code(Borrowed(".md")) [3903..3908]
                      Text(Borrowed(" ending, we seek for note ")) [3908..3934]
                      Code(Borrowed("Figure1.jpg")) [3934..3947]
                Item [3948..3997]
                  Code(Borrowed("[[Figure1.jpg.md.md]]")) [3950..3973]
                  Text(Borrowed(": ")) [3973..3975]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg.md.md"), title: Borrowed(""), id: Borrowed("") } [3975..3995]
                    Text(Borrowed("Figure1.jpg.md.md")) [3977..3994]
                Item [3997..4136]
                  Code(Borrowed("[[Figure1#2.jpg]]")) [3999..4018]
                  Text(Borrowed(": ")) [4018..4020]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1#2.jpg"), title: Borrowed(""), id: Borrowed("") } [4020..4036]
                    Text(Borrowed("Figure1#2.jpg")) [4022..4035]
                  List(None) [4037..4136]
                    Item [4037..4136]
                      Text(Borrowed("understood as note and points to note Figure1 (fallback to note after failing finding heading)")) [4041..4135]
                Item [4136..4275]
                  Code(Borrowed("[[Figure1|2.jpg]]")) [4138..4157]
                  Text(Borrowed(": ")) [4157..4159]
                  Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Figure1"), title: Borrowed(""), id: Borrowed("") } [4159..4175]
                    Text(Borrowed("2.jpg")) [4169..4174]
                  List(None) [4176..4275]
                    Item [4176..4275]
                      Text(Borrowed("understood as note and points to note Figure1 (fallback to note after failing finding heading)")) [4180..4274]
                Item [4275..4335]
                  Code(Borrowed("[[Figure1^2.jpg]]")) [4277..4296]
                  Text(Borrowed(": ")) [4296..4298]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1^2.jpg"), title: Borrowed(""), id: Borrowed("") } [4298..4314]
                    Text(Borrowed("Figure1^2.jpg")) [4300..4313]
                  List(None) [4315..4335]
                    Item [4315..4335]
                      Text(Borrowed("points to image")) [4319..4334]
                Item [4335..4410]
                  Text(Borrowed("↳ when there")) [4337..4351]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4351..4352]
                  Text(Borrowed("s ")) [4352..4354]
                  Code(Borrowed(".md")) [4354..4359]
                  Text(Borrowed(", it")) [4359..4363]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4363..4364]
                  Text(Borrowed("s removed and limit to the searching of notes")) [4364..4409]
                Item [4410..4749]
                  Code(Borrowed("[[dir/]]")) [4412..4422]
                  Text(Borrowed(": ")) [4422..4424]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/"), title: Borrowed(""), id: Borrowed("") } [4424..4431]
                    Text(Borrowed("dir/")) [4426..4430]
                  List(None) [4432..4749]
                    Item [4432..4440]
                      Text(Borrowed("BUG")) [4436..4439]
                    Item [4439..4501]
                      Text(Borrowed("when clicking it, it will create ")) [4443..4476]
                      Code(Borrowed("dir")) [4476..4481]
                      Text(Borrowed(" note if not exists")) [4481..4500]
                    Item [4500..4538]
                      Text(Borrowed("create ")) [4504..4511]
                      Code(Borrowed("dir 1.md")) [4511..4521]
                      Text(Borrowed(" if ")) [4521..4525]
                      Code(Borrowed("dir")) [4525..4530]
                      Text(Borrowed(" exists")) [4530..4537]
                    Item [4537..4586]
                      Text(Borrowed("create ")) [4541..4548]
                      Code(Borrowed("dir {n+1}.md")) [4548..4562]
                      Text(Borrowed(" if ")) [4562..4566]
                      Code(Borrowed("dir {n}.md")) [4566..4578]
                      Text(Borrowed(" exists")) [4578..4585]
                    Item [4585..4749]
                      Text(Borrowed("I guess the logic is:")) [4589..4610]
                      List(None) [4611..4749]
                        Item [4611..4675]
                          Text(Borrowed("there")) [4615..4620]
                          Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4620..4621]
                          Text(Borrowed("s no file named ")) [4621..4637]
                          Code(Borrowed("dir/")) [4637..4643]
                          Text(Borrowed(", Obsidian try to create a note")) [4643..4674]
                        Item [4675..4702]
                          Text(Borrowed("it removes ")) [4679..4690]
                          Code(Borrowed("/")) [4690..4693]
                          Text(Borrowed(" and ")) [4693..4698]
                          Code(Borrowed("\\")) [4698..4701]
                        Item [4702..4749]
                          Text(Borrowed("if there exists one, it add integer suffix")) [4706..4748]
                Item [4749..5280]
                  Text(Borrowed("matching of nested dirs only match ancestor-descendant relationship")) [4751..4818]
                  List(None) [4818..5280]
                    Item [4818..4897]
                      Code(Borrowed("[[dir/inner_dir/note_in_inner_dir]]")) [4822..4859]
                      Text(Borrowed(": ")) [4859..4861]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/inner_dir/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [4861..4895]
                        Text(Borrowed("dir/inner_dir/note_in_inner_dir")) [4863..4894]
                    Item [4896..4967]
                      Code(Borrowed("[[inner_dir/note_in_inner_dir]]")) [4900..4933]
                      Text(Borrowed(": ")) [4933..4935]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("inner_dir/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [4935..4965]
                        Text(Borrowed("inner_dir/note_in_inner_dir")) [4937..4964]
                    Item [4966..5025]
                      Code(Borrowed("[[dir/note_in_inner_dir]]")) [4970..4997]
                      Text(Borrowed(": ")) [4997..4999]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [4999..5023]
                        Text(Borrowed("dir/note_in_inner_dir")) [5001..5022]
                    Item [5024..5060]
                      Text(Borrowed("↳ all points to the same note")) [5028..5059]
                    Item [5059..5260]
                      Code(Borrowed("[[random/note_in_inner_dir]]")) [5063..5093]
                      Text(Borrowed(": ")) [5093..5095]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("random/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [5095..5122]
                        Text(Borrowed("random/note_in_inner_dir")) [5097..5121]
                      List(None) [5124..5260]
                        Item [5124..5146]
                          Text(Borrowed("this has no match")) [5128..5145]
                        Item [5146..5199]
                          Text(Borrowed("it will try to understand the file name and path")) [5150..5198]
                        Item [5199..5260]
                          Text(Borrowed("mkdir and touch file (in contrast to the case of ")) [5203..5252]
                          Code(Borrowed("dir/")) [5252..5258]
                          Text(Borrowed(")")) [5258..5259]
                    Item [5259..5280]
                      Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("inner_dir/hi"), title: Borrowed(""), id: Borrowed("") } [5263..5278]
                        Text(Borrowed("inner_dir/hi")) [5265..5277]
                Item [5280..5333]
                  Code(Borrowed("[[dir/indir_same_name]]")) [5282..5307]
                  Text(Borrowed(": ")) [5307..5309]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/indir_same_name"), title: Borrowed(""), id: Borrowed("") } [5309..5331]
                    Text(Borrowed("dir/indir_same_name")) [5311..5330]
                Item [5333..5429]
                  Code(Borrowed("[[indir_same_name]]")) [5335..5356]
                  Text(Borrowed(": ")) [5356..5358]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("indir_same_name"), title: Borrowed(""), id: Borrowed("") } [5358..5376]
                    Text(Borrowed("indir_same_name")) [5360..5375]
                  List(None) [5377..5429]
                    Item [5377..5429]
                      Text(Borrowed("points to ")) [5381..5391]
                      Code(Borrowed("indir_same_name")) [5391..5408]
                      Text(Borrowed(", not the in dir one")) [5408..5428]
                Item [5429..5483]
                  Code(Borrowed("[[indir2]]")) [5432..5444]
                  Text(Borrowed(": ")) [5444..5446]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("indir2"), title: Borrowed(""), id: Borrowed("") } [5446..5455]
                    Text(Borrowed("indir2")) [5448..5454]
                  List(None) [5457..5483]
                    Item [5457..5483]
                      Text(Borrowed("points to ")) [5460..5470]
                      Code(Borrowed("dir/indir2")) [5470..5482]
                Item [5483..5596]
                  Code(Borrowed("[[Something]]")) [5485..5500]
                  Text(Borrowed(": ")) [5500..5502]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Something"), title: Borrowed(""), id: Borrowed("") } [5502..5514]
                    Text(Borrowed("Something")) [5504..5513]
                  List(None) [5515..5596]
                    Item [5515..5596]
                      Text(Borrowed("there exists a ")) [5519..5534]
                      Code(Borrowed("Something")) [5534..5545]
                      Text(Borrowed(" file, but this will points to note ")) [5545..5581]
                      Code(Borrowed("Something.md")) [5581..5595]
                Item [5596..5716]
                  Code(Borrowed("[[unsupported_text_file.txt]]")) [5598..5629]
                  Text(Borrowed(": ")) [5629..5631]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("unsupported_text_file.txt"), title: Borrowed(""), id: Borrowed("") } [5631..5659]
                    Text(Borrowed("unsupported_text_file.txt")) [5633..5658]
                  List(None) [5660..5716]
                    Item [5660..5716]
                      Text(Borrowed("points to text file, which is of unsupported format")) [5664..5715]
                Item [5716..5777]
                  Code(Borrowed("[[a.joiwduvqneoi]]")) [5718..5738]
                  Text(Borrowed(": ")) [5738..5740]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("a.joiwduvqneoi"), title: Borrowed(""), id: Borrowed("") } [5740..5757]
                    Text(Borrowed("a.joiwduvqneoi")) [5742..5756]
                  List(None) [5758..5777]
                    Item [5758..5777]
                      Text(Borrowed("points to file")) [5762..5776]
                Item [5777..5876]
                  Code(Borrowed("[[Note 1]]")) [5779..5791]
                  Text(Borrowed(": ")) [5791..5793]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 1"), title: Borrowed(""), id: Borrowed("") } [5793..5802]
                    Text(Borrowed("Note 1")) [5795..5801]
                  List(None) [5803..5876]
                    Item [5803..5876]
                      Text(Borrowed("even if there exists a file named ")) [5807..5841]
                      Code(Borrowed("Note 1")) [5841..5849]
                      Text(Borrowed(", this points to the note")) [5849..5874]
              Paragraph [5876..5956]
                Code(Borrowed("![[Figure1.jpg]]")) [5876..5894]
                Text(Borrowed(": ")) [5894..5896]
                Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg"), title: Borrowed(""), id: Borrowed("") } [5896..5911]
                  Text(Borrowed("Figure1.jpg")) [5899..5910]
                SoftBreak [5912..5913]
                Code(Borrowed("[[empty_video.mp4]]")) [5913..5934]
                Text(Borrowed(": ")) [5934..5936]
                Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("empty_video.mp4"), title: Borrowed(""), id: Borrowed("") } [5936..5954]
                  Text(Borrowed("empty_video.mp4")) [5938..5953]
              Heading { level: H2, id: None, classes: [], attrs: [] } [5957..5963]
                Text(Borrowed("L2")) [5960..5962]
              Heading { level: H3, id: None, classes: [], attrs: [] } [5964..5971]
                Text(Borrowed("L3")) [5968..5970]
              Heading { level: H4, id: None, classes: [], attrs: [] } [5971..5979]
                Text(Borrowed("L4")) [5976..5978]
              Heading { level: H3, id: None, classes: [], attrs: [] } [5979..5994]
                Text(Borrowed("Another L3")) [5983..5993]
              Rule [5995..5999]
              Heading { level: H2, id: None, classes: [], attrs: [] } [5999..6003]
            "########);
}
