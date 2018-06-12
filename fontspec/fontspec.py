"""Font specification generator.

This module exposes functionality to generate a font specification
that is used by the pymble server to convert an images into colored ASCII art.

Each font has a different representation of a characters and as a result
the same character in different fonts is represents using different set of
pixels. This means that the same character in different fonts could have
a different 'brightness' value.

For more presice conversion of an image into ASCII art it is important to
know the font that is used to output the resulting ASCII art.

The responsibilities of this module is to dynamically generate the image bitmaps
of characters for the specified font and evaluate the brightness of each individual
character. The final output is the generated font specification that basically
is a map of characters and their brightness. 

Example:
    TBD

"""