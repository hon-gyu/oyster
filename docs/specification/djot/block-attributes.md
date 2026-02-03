Block attributes

To attach attributes to a block-level element, put the attributes on the line immediately before the block. Block attributes have the same syntax as inline attributes, but if they don’t fit on one line, subsequent lines must be indented. Repeated attribute specifiers can be used, and the attributes will accumulate.

{#water}
{.important .large}
Don't forget to turn off the water!

{source="Iliad"}
> Sing, muse, of the wrath of Achilles

<p class="important large" id="water">Don&rsquo;t forget to turn off the water!</p>
<blockquote source="Iliad">
<p>Sing, muse, of the wrath of Achilles</p>
</blockquote>
