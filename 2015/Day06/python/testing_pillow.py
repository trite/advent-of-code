import glob
from PIL import Image, ImageDraw

def test_image(offset: int, name: str) -> None:
    im = Image.new('RGB', (500, 500), (64, 64, 64))
    draw = ImageDraw.Draw(im)

    draw.rectangle((50, 50, 450, 450), fill=(200, 100, 50))
    draw.rectangle((25, 25, 100, 100), fill=(100, 200, 50))
    draw.rectangle((250, 100 + offset, 400, 250 + offset), fill=(50, 200, 100))

    im.save(name, quality=95)

imgPath = '2015/Day06/python/images'
imgPrefix = 'test_animation'

for o, i in zip(range(0, 250, 10), range(10000)):
    test_image(o, f'{imgPath}/{imgPrefix}{i:03d}.jpg')

inputImgs = f'{imgPath}/{imgPrefix}*.jpg'
img, *imgs = [Image.open(f) for f in sorted(glob.glob(inputImgs))]

img.save(
    fp = f'{imgPath}/{imgPrefix}.gif',
    format = 'GIF',
    append_images = imgs,
    save_all = True,
    duration = 50, # frame duration in ms
    loop = 0
)