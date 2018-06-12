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

from PIL import Image, ImageDraw, ImageFont


def create_char_image(char, font_path,
                      image_size=(90, 160), font_size=120,
                      background=(0, 0, 0), foreground=(255, 255, 255)):
    """Renders the specified char to an image using the provided font.

    Args:
        char (str): The character to render.
        font_path (str): The path to the ttf or otf font file.
        image_size ((int, int)): Size of the generated image. Defaults to (90, 160).
        font_size (int): Font size. Defaults to 120.
        background ((int, int, int)): Background color of the generated image (R, G, B).
            Defaults to black color (0, 0, 0).
        foreground ((int, int, int)): The color of the rendered character (R, G, B).
            Defaults to white color (255, 255, 255).

    Returns:
        PIL.Image: The image with the rendered char.

    """

    char_image = Image.new('RGB', image_size, background)
    char_font  = ImageFont.truetype(font_path, font_size)
    draw       = ImageDraw.Draw(char_image)

    draw.text((1, 0), char, font=char_font, fill=foreground)

    return char_image