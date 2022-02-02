from typing import Tuple
import numpy as np
from PIL import Image

pixels = [
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
    [0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2],
    [0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3],
    [0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4],
    [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5],
    [0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6],
    [0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7],
    [0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8],
    [0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9],
]

array = np.array(pixels, dtype=float) # type: ignore

image = Image.fromarray(array, mode="L") # type: ignore

image.save('test.png')

def random_img(output: str, width: int, height: int) -> None:

    array = np.random.random_integers(0,255, (height,width,3)) # type: ignore

    array = np.array(array, dtype=np.uint8) # type: ignore
    img = Image.fromarray(array) # type: ignore
    img.save(output)


random_img('random.png', 1000, 1000)

def to_pixel(i: int) -> Tuple[int,int,int]:
    r = int((float(i) / 1000) * 255)
    return r,r,r

# f = lambda x, y: to_pixel(x)
def func(x: int, y: int) -> Tuple[int,int,int]:
    return to_pixel(x)

arr = np.fromfunction(np.vectorize(func), (1000, 1000), dtype = np.uint8) # type: ignore
Image.fromarray(arr).save('gradual.png') # type: ignore