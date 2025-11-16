pub fn percent_decode(url: &str) -> String {
    percent_encoding::percent_decode_str(url)
        .decode_utf8_lossy()
        .to_string()
}

#[allow(dead_code)]
pub fn percent_encode(url: &str) -> String {
    percent_encoding::utf8_percent_encode(
        url,
        percent_encoding::NON_ALPHANUMERIC,
    )
    .to_string()
    .replace("%23", "#") // Preserve # for heading anchors
    .replace("%2F", "/") // Preserve / for file paths
}
