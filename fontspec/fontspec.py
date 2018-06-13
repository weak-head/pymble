"""Font specification generator.

This module exposes functionality to generate a font specification
that is used by the pymble server to convert images into colored ASCII art.

Each font has a different representation of the characters and as a result
the same character in the different fonts could be represented using different set of
pixels. As a consequence it means that the same character rendered using
different fonts could have a different 'brightness' value.

For more presice conversion of an image into ASCII art it is important to
know the font that is used to output the resulting ASCII art.

The responsibilities of this module is to dynamically generate the image bitmaps
of characters for the specified font and evaluate the brightness of each individual
character. The final output is the generated font specification that basically
is a map of characters and their brightness. 

Example:
    Generate the default fontspec::

        $ python fontspec.py

    Getting usage help::

        $ python fontspec.py -h

    Fontspec for custom dictionary and font to custom file:

        $ python fontspec.py -d "1234567890" -f "monofur.ttf" -o "monofur_num_spec.txt"

"""

import argparse
from PIL import Image, ImageDraw, ImageFont


def create_char_image(char, font_path,
                      image_size=(90, 160), font_size=120,
                      background=(0, 0, 0), foreground=(255, 255, 255)):
    """Renders the specified character of the provided font to an image.

    Args:
        char (str): The character to render as an image.
        font_path (str): The path to the ttf or otf font file.
        image_size ((int, int)): Dimensions of the generated image. Defaults to (90, 160).
        font_size (int): The font size that is used to render the image.
            In practice big letters rendered on hi dimension image gives more
            accurate results. Defaults to 120.
        background ((int, int, int)): Background color of the generated image (R, G, B).
            Defaults to black color (0, 0, 0).
        foreground ((int, int, int)): The color of the rendered character (R, G, B).
            Defaults to white color (255, 255, 255).

    Returns:
        PIL.Image: The image of the rendered char.

    """

    char_image = Image.new('RGB', image_size, background)
    char_font  = ImageFont.truetype(font_path, font_size)
    draw       = ImageDraw.Draw(char_image)

    draw.text((1, 0), char, font=char_font, fill=foreground)

    return char_image


def char_brightness(char_image):
    """Given the image with the rendered character, computes the total
    average brightness.

    Args:
        char_image (Image): The image with the rendered char.

    Returns:
        int: total average brightness of the image (basically of the char).

    """

    total_brightness = 0
    width, height = char_image.size
    for w in range(width):
        for h in range(height):
            r, g, b = char_image.getpixel((w, h))
            total_brightness += (r + g + b) / 3

    return total_brightness / (width * height)


def eval_brightness_dict(dictionary, font_path):
    """Given a collection of characters and a font,
    computes the brightness of every character in the
    collection.

    Args:
        dictionary (list): A collection of characters that would be used
            as a dictionary during the image conversion to ASCII art.
        font_path (str): The path to the ttf or otf font file.

    Returns:
        dict: A map of characters to brightness.

    """

    return {c: char_brightness(create_char_image(c, font_path)) for c in dictionary}


def normalize_brightness_dict(brightness_dict):
    """Usually the distribution of the character brightness for a given font
    is not as diverse as we would like it to be. This results in a pretty
    poor result during the image to ASCII art conversion (if used as-is).

    Because of this it's much better to normalize the brightness distribution.
    Normalization widens the distribution ranges to 8-bit 0-255 range.

    Args:
        brightness_dict (dict): A map of characters to brightness.

    Returns:
        dict: normalized map of characters to brightness.

    """

    b_min, b_max = 255, 0
    for brightness in brightness_dict.values():
        b_min = min(b_min, brightness)
        b_max = max(b_max, brightness)

    def widen(b):
        """Using the min and max bounds to widen the char brightness"""
        return int(round( ( (b - b_min) / (b_max - b_min) ) * 255 ))

    return {char: widen(brightness) for char, brightness in brightness_dict.items()}


def save_fontspec(brightness_dict, file_path='spec.txt'):
    """Given the character brightness dictionary, outputs it
    to simple text file. The content of the generated file could be copy-pasted
    directly into haskell source code.

    Of course it is possible to dynamically generate the fontspecs and load
    them directly from fontspec file in pymble, but we want to keep it simple
    stupid and do not overload it with useless functionality.

    Args:
        brightness_dict (dict): A map of characters to brightness.
        file_path (str): Path to the file to save the generated fontspec.
            Defaults to 'spec.txt'.
    """
    with open(file_path, 'w+') as f:
        f.write('[ ')

        first = True
        for char, brightness in sorted(brightness_dict.items(), key=lambda x: x[1]):
            if not first: f.write(', ')
            f.write("('{char}', {brightness})\n".format(char=char, brightness=brightness))
            first = False

        f.write(']')


def generate_fontspec(dictionary, font_path='cour.ttf', file_path='spec.txt'):
    """Generates and saves to file the specification of the specified font
    for the given collection of characters.

    Args:
        dictionary (list): A collection of characters that would be used
            as a dictionary during the image conversion to ASCII art.
        font_path (str): Path to the font that is used to render the
            dictionary with characters. Defaults to 'cour.ttf'.
        file_path (str): Path to the file that would have the
            generated fontspec. Defaults to 'spec.txt'.
        
    """
    brightness_dict      = eval_brightness_dict(dictionary, font_path)
    nomalized_brightness = normalize_brightness_dict(brightness_dict)

    save_fontspec(nomalized_brightness, file_path)


def create_argparser():
    """Creates and initializes the script argument parser."""

    parser = argparse.ArgumentParser(
                        description="""Generate font specification for the given dictionary.
                                    The font specification is used by the pymble server to
                                    render ASCII art.
                                    """)

    parser.add_argument('-d',
                        type=str,
                        default='TBD',
                        dest='dictionary',
                        help="""collection of characters that is used as a dictionary
                             during the ASCII art generation (default: 'TBD')
                             """)

    parser.add_argument('-f',
                        type=str,
                        default='cour.ttf',
                        dest='font',
                        help="""path to the font that is used to render the generated ASCII art
                             (default: 'cour.ttf')
                             """)

    parser.add_argument('-o',
                        type=str,
                        default='spec.txt',
                        dest='filepath',
                        help="""path to the generated file with the resulting font specification
                             for the given dictionary (default: 'spec.txt')
                             """)

    return parser


if __name__ == '__main__':

    parser = create_argparser()
    args = parser.parse_args()

    generate_fontspec(args.dictionary, args.font, args.filepath)