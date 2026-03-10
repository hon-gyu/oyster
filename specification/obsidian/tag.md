# tag

<https://help.obsidian.md/tags>

`#something` will be a tag

# Nested tags
Nested tags define tag hierarchies that make it easier to find and filter related tags.
Create nested tags by using forward slashes (/) in the tag name, for example #inbox/to-read and #inbox/processing.

    In Search, tag:inbox will match #inbox as well as all nested tags such as #inbox/to-read.
    In the Tags view, nested tags are shown as belonging to their parent tag.
    In Bases, nested tags are recognized by the `hasTag` function, so file.hasTag("a") will match both #a and #a/b.

# Tag format
You can use any of the following characters in your tags:

    Alphabetical letters
    Numbers
    Underscore (_)
    Hyphen (-)
    Forward slash (/) for Nested tags

Tags must contain at least one non-numerical character. For example, #1984 isn't a valid tag, but #y1984 is.
