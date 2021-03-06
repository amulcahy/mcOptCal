/*
Copyright (c) 2013 Anthony Mulcahy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
package com.dragongate_technologies.mcOptCal

object defaultData {

 val samplePriceArray: Array[Array[Double]] = Array(
Array( 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0 ),
Array( 36.0,  36.2,  35.2,  36.1,  38.7,  39.3,  38.9,  39.3,  41.1,  40.0,  40.3,  40.7,  40.1,  41.4,  41.0,  41.4,  41.8,  41.6,  42.6,  42.8,  39.2,  37.9,  39.1,  38.0,  38.1,  39.5,  38.3,  38.8,  38.9,  40.6,  42.1,  42.6,  43.3,  42.9,  43.7,  45.8,  45.3,  44.6,  43.5,  43.9,  45.3,  44.5,  44.5,  43.2,  41.9,  41.6,  41.9,  42.6,  42.0,  42.5,  41.7 ),
Array( 36.0,  36.1,  35.2,  36.1,  37.0,  35.7,  36.4,  37.3,  37.4,  36.9,  39.4,  39.7,  38.7,  39.1,  37.4,  36.1,  35.7,  37.0,  37.0,  39.9,  40.5,  39.9,  38.4,  39.8,  39.9,  39.6,  39.4,  40.0,  39.5,  38.3,  37.2,  36.7,  36.8,  37.2,  37.9,  39.0,  39.7,  40.9,  41.1,  41.5,  42.9,  43.7,  44.3,  45.2,  45.6,  45.6,  44.6,  47.1,  45.9,  46.6,  44.6 ),
Array( 36.0,  36.0,  37.0,  36.1,  35.3,  36.6,  35.9,  35.1,  35.1,  35.6,  33.5,  33.2,  34.2,  33.8,  35.4,  36.7,  37.2,  35.9,  36.0,  33.5,  33.0,  33.6,  35.0,  33.8,  33.7,  34.0,  34.3,  33.8,  34.3,  35.4,  36.5,  37.1,  37.1,  36.7,  36.1,  35.1,  34.6,  33.6,  33.5,  33.2,  32.2,  31.7,  31.3,  30.7,  30.5,  30.6,  31.3,  29.7,  30.5,  30.1,  31.5 ),
Array( 36.0,  37.6,  38.9,  38.9,  39.3,  37.7,  39.7,  39.5,  39.5,  39.8,  40.2,  38.6,  37.8,  38.6,  39.7,  40.2,  42.2,  42.9,  43.1,  44.4,  44.8,  45.3,  46.8,  44.6,  46.6,  46.1,  47.3,  48.5,  46.6,  47.1,  47.2,  46.7,  46.2,  45.0,  44.8,  42.9,  43.3,  44.5,  42.8,  41.4,  39.9,  39.8,  39.4,  40.6,  40.4,  41.2,  40.7,  38.8,  38.1,  40.3,  42.0 ),
Array( 36.0,  34.5,  33.4,  33.5,  33.2,  34.6,  33.0,  33.1,  33.3,  33.1,  32.8,  34.2,  34.9,  34.3,  33.3,  33.0,  31.5,  31.0,  30.9,  30.1,  29.9,  29.6,  28.7,  30.1,  28.9,  29.2,  28.5,  27.9,  29.1,  28.8,  28.8,  29.1,  29.5,  30.4,  30.5,  32.0,  31.7,  30.9,  32.2,  33.3,  34.6,  34.7,  35.2,  34.2,  34.5,  33.8,  34.3,  36.0,  36.8,  34.8,  33.4 ),
Array( 36.0,  34.4,  34.5,  35.7,  36.0,  34.9,  37.2,  36.0,  35.1,  34.2,  35.0,  34.0,  33.9,  34.4,  34.6,  33.1,  32.4,  33.6,  32.8,  33.3,  33.4,  33.3,  32.9,  34.1,  33.7,  33.4,  34.8,  35.8,  34.3,  33.2,  33.4,  32.6,  33.3,  33.9,  32.5,  32.1,  30.2,  31.0,  31.1,  30.8,  30.1,  29.0,  28.9,  29.4,  29.9,  29.9,  29.4,  27.7,  27.6,  27.8,  27.9 ),
Array( 36.0,  37.7,  37.7,  36.5,  36.3,  37.4,  35.2,  36.4,  37.4,  38.5,  37.6,  38.8,  39.0,  38.5,  38.3,  40.1,  41.0,  39.7,  40.6,  40.2,  40.0,  40.2,  40.8,  39.4,  39.9,  40.4,  38.9,  37.8,  39.5,  40.9,  40.7,  41.7,  41.0,  40.3,  42.1,  42.7,  45.4,  44.4,  44.3,  44.7,  45.9,  47.8,  47.9,  47.3,  46.4,  46.5,  47.5,  50.4,  50.7,  50.4,  50.3 ),
Array( 36.0,  36.3,  36.6,  36.8,  37.6,  39.2,  39.1,  41.0,  41.7,  41.2,  41.6,  43.2,  43.9,  43.9,  43.7,  43.9,  43.9,  43.8,  43.8,  44.0,  42.0,  44.8,  45.5,  46.4,  48.4,  50.5,  47.4,  47.3,  45.6,  45.1,  46.6,  45.4,  46.9,  47.2,  47.4,  50.1,  49.5,  49.7,  49.9,  52.8,  53.2,  52.6,  51.6,  49.9,  50.7,  51.2,  50.5,  49.9,  50.7,  54.4,  54.5 ),
Array( 36.0,  35.8,  35.5,  35.3,  34.7,  33.3,  33.5,  32.0,  31.5,  31.9,  31.7,  30.5,  30.1,  30.1,  30.3,  30.2,  30.3,  30.4,  30.4,  30.4,  31.9,  29.9,  29.5,  29.0,  27.8,  26.7,  28.5,  28.6,  29.7,  30.1,  29.2,  30.0,  29.1,  29.0,  28.9,  27.4,  27.7,  27.7,  27.6,  26.1,  26.0,  26.3,  26.8,  27.8,  27.4,  27.2,  27.6,  28.0,  27.6,  25.8,  25.8 ),
Array( 36.0,  35.8,  36.6,  38.3,  39.5,  39.4,  38.8,  40.1,  38.7,  37.8,  37.9,  40.5,  39.5,  37.7,  39.0,  38.2,  37.4,  38.4,  40.3,  39.4,  40.7,  40.3,  42.2,  40.1,  39.4,  38.7,  39.3,  39.1,  40.3,  39.8,  38.6,  38.3,  39.0,  39.4,  39.2,  39.3,  39.5,  39.4,  39.6,  38.5,  39.7,  40.5,  39.8,  40.6,  40.0,  39.8,  40.0,  41.2,  42.6,  42.0,  40.4 ),
Array( 36.0,  36.2,  35.5,  34.0,  33.0,  33.2,  33.8,  32.7,  33.9,  34.8,  34.7,  32.6,  33.5,  35.1,  34.0,  34.8,  35.5,  34.7,  33.1,  33.9,  32.8,  33.3,  31.8,  33.6,  34.2,  34.9,  34.3,  34.6,  33.7,  34.1,  35.2,  35.6,  35.0,  34.7,  34.9,  34.9,  34.8,  34.9,  34.8,  35.8,  34.8,  34.2,  34.9,  34.2,  34.8,  35.0,  34.9,  33.9,  32.8,  33.4,  34.8 ),
Array( 36.0,  35.6,  33.6,  33.2,  33.7,  35.1,  33.5,  34.6,  36.0,  35.0,  35.5,  34.0,  33.3,  34.3,  35.9,  34.4,  33.8,  34.4,  35.3,  36.2,  37.0,  37.7,  36.6,  36.8,  36.9,  37.4,  37.6,  37.6,  38.1,  39.1,  40.2,  38.6,  38.1,  38.0,  38.5,  37.0,  37.4,  37.4,  39.2,  38.2,  39.8,  41.3,  40.4,  38.9,  39.1,  39.8,  43.1,  41.6,  41.2,  40.1,  40.6 ),
Array( 36.0,  36.5,  38.7,  39.3,  38.7,  37.2,  39.1,  37.9,  36.5,  37.6,  37.1,  38.7,  39.6,  38.6,  36.9,  38.6,  39.4,  38.7,  37.7,  36.9,  36.1,  35.5,  36.7,  36.5,  36.5,  36.1,  36.0,  35.9,  35.6,  34.7,  33.8,  35.3,  35.8,  36.0,  35.6,  37.0,  36.7,  36.8,  35.1,  36.1,  34.7,  33.5,  34.3,  35.7,  35.6,  35.0,  32.4,  33.6,  34.0,  34.9,  34.6 ),
Array( 36.0,  34.7,  35.0,  35.0,  35.0,  36.1,  36.5,  37.3,  36.8,  36.3,  34.2,  33.3,  34.0,  35.7,  35.6,  37.2,  37.8,  37.7,  37.5,  37.8,  37.3,  37.9,  38.6,  38.6,  38.8,  38.4,  39.0,  39.8,  40.4,  40.3,  43.3,  45.6,  44.6,  46.2,  48.2,  48.8,  51.5,  49.7,  48.5,  47.9,  49.9,  51.6,  52.3,  50.9,  49.3,  51.5,  53.6,  52.0,  53.4,  54.4,  54.1 ),
Array( 36.0,  37.4,  37.2,  37.2,  37.2,  36.2,  35.9,  35.1,  35.7,  36.2,  38.5,  39.6,  38.9,  37.0,  37.2,  35.7,  35.2,  35.3,  35.6,  35.3,  35.9,  35.4,  34.8,  34.8,  34.7,  35.2,  34.6,  34.0,  33.6,  33.6,  31.4,  29.9,  30.6,  29.6,  28.4,  28.1,  26.7,  27.7,  28.4,  28.8,  27.7,  26.8,  26.5,  27.3,  28.2,  27.1,  26.0,  26.9,  26.2,  25.8,  25.9 ),
Array( 36.0,  35.3,  36.0,  35.9,  35.8,  35.1,  35.8,  35.8,  38.1,  39.3,  39.5,  39.9,  39.4,  38.9,  39.5,  40.6,  40.6,  39.1,  39.1,  39.3,  37.6,  36.1,  34.7,  33.6,  32.7,  32.8,  33.4,  34.0,  34.1,  34.9,  35.3,  35.9,  36.5,  35.3,  35.9,  36.1,  37.4,  37.9,  36.0,  36.8,  35.2,  35.4,  35.2,  35.7,  34.0,  32.5,  33.3,  34.3,  33.7,  33.5,  34.6 ),
Array( 36.0,  36.7,  36.1,  36.2,  36.4,  37.2,  36.6,  36.6,  34.5,  33.5,  33.3,  33.1,  33.5,  34.0,  33.5,  32.7,  32.8,  34.0,  34.1,  34.0,  35.6,  37.1,  38.7,  40.0,  41.2,  41.1,  40.4,  39.8,  39.8,  38.9,  38.6,  38.0,  37.4,  38.7,  38.1,  37.9,  36.8,  36.3,  38.3,  37.5,  39.2,  39.1,  39.4,  38.8,  40.9,  42.8,  41.9,  40.7,  41.5,  41.9,  40.6 ),
Array( 36.0,  36.5,  36.4,  37.8,  38.6,  39.6,  37.8,  36.9,  36.8,  37.3,  38.0,  36.3,  35.7,  37.9,  39.6,  40.5,  40.6,  40.5,  41.1,  40.3,  39.1,  38.8,  38.3,  36.7,  37.0,  36.4,  35.7,  36.0,  36.3,  35.3,  34.7,  35.3,  38.0,  37.7,  38.6,  38.6,  36.1,  35.9,  34.5,  36.1,  36.8,  37.3,  37.0,  37.4,  36.3,  36.6,  35.1,  34.7,  34.0,  34.3,  35.0 ),
Array( 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0 )
)

}
