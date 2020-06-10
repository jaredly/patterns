import * as d3Color from 'd3-color';
import * as d3Interpolate from 'd3-interpolate';

const p = (function () {
    // prettier-ignore
    const permutation = [
      151,160,137,91,90,15,
      131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
      190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
      88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
      77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
      102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
      135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
      5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
      223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
      129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
      251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
      49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
      138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
    ];

    // avoid overflow
    const p = new Uint8Array(512);
    for (let i = 0; i < 256; i++) {
        p[i] = p[256 + i] = permutation[i];
    }

    return p;
})();

function perlin2(x, y, px, py) {
    // Integer part of x, y
    let xi = Math.floor(x);
    let yi = Math.floor(y);

    // Fractional part of x, y
    let xf = x - xi;
    let yf = y - yi;

    // Wrap to 0..px-1 and wrap to 0..255
    let xi1 = (px ? (xi + 1) % px : xi + 1) & 0xff;
    let yi1 = (py ? (yi + 1) % py : yi + 1) & 0xff;

    xi = (px ? xi % px : xi) & 0xff;
    yi = (py ? yi % py : yi) & 0xff;

    let s = fade(xf);
    let t = fade(yf);

    return lerp(
        s,
        lerp(
            t,
            grad2(p[xi + p[yi]], xf, yf),
            grad2(p[xi + p[yi1]], xf, yf - 1),
        ),
        lerp(
            t,
            grad2(p[xi1 + p[yi]], xf - 1, yf),
            grad2(p[xi1 + p[yi1]], xf - 1, yf - 1),
        ),
    );
}

function fade(t) {
    return t * t * t * (t * (t * 6 - 15) + 10);
}

function grad2(hash, x, y) {
    const h = hash & 7; // Convert low 3 bits of hash code
    const u = h < 4 ? x : y; // into 8 simple gradient directions,
    const v = h < 4 ? y : x; // and compute the dot product with (x,y).
    return (h & 1 ? -u : u) + (h & 2 ? -v : v);
}

function lerp(t, a, b) {
    return a + t * (b - a);
}

const cache = {};

export function makeImage(w, h, color) {
    const key = `${w}:${h}:${color}`;
    if (!cache[key]) {
        const canvas = document.createElement('canvas');
        canvas.width = w;
        canvas.height = h;
        const ctx = canvas.getContext('2d');
        // const ctx = DOM.context2d(w, h, 1);
        const outputPixels = ctx.getImageData(0, 0, w, h);
        fillImage(outputPixels, w, h, color);
        ctx.putImageData(outputPixels, 0, 0);
        ctx.canvas.style.maxWidth = '100%';
        cache[key] = ctx.canvas.toDataURL();
    }
    return cache[key];
}

function getPixelIndex(x, y, width) {
    return y * width * 4 + x * 4;
}

const scale = 0.3;
const scale2 = 0.004;
const scale3 = 0.001;
// const octaves = 2
const offset = 0;

function fillImage(outputPixels, width, height, color) {
    const light = color; // "#0f0";
    const dark = d3Color.rgb(light).darker(2);
    const scalec = d3Interpolate.interpolateRgb(light, dark);
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const index = getPixelIndex(x, y, width);
            const r1 = perlin2(
                x * scale,
                y * scale,
                width * scale,
                height * scale,
            );
            const r2 = perlin2(
                offset + x * scale2,
                y * scale2,
                width * scale2,
                height * scale2,
            );
            const r3 = perlin2(
                x * scale3,
                offset + y * scale3,
                width * scale3,
                height * scale3,
            );
            const r = (r1 + r2 + r3) / 3;
            const amt = Math.pow(r / 2 + 0.5, 2);
            const color = d3Color.rgb(scalec(amt));
            outputPixels.data[index + 0] = color.r;
            outputPixels.data[index + 1] = color.g;
            outputPixels.data[index + 2] = color.b;
            outputPixels.data[index + 3] = 255;
        }
    }
}
