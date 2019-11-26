/* Terminal colors (16 first used in escape sequence) */
const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#4e4e4e", /* black   */
  [1] = "#a9635d", /* red     */
  [2] = "#468459", /* green   */
  [3] = "#84763d", /* yellow  */
  [4] = "#5879af", /* blue    */
  [5] = "#9c6594", /* magenta */
  [6] = "#008592", /* cyan    */
  [7] = "#cfcfcf", /* white   */

  /* 8 bright colors */
  [8]  = "#777777", /* black   */
  [9]  = "#ffbbb2", /* red     */
  [10] = "#9ddeaf", /* green   */
  [11] = "#e0ce91", /* yellow  */
  [12] = "#b3d1ff", /* blue    */
  [13] = "#fabdf0", /* magenta */
  [14] = "#79e0ed", /* cyan    */
  [15] = "#ffffff", /* white   */

  /* special colors */
  [256] = "#292929", /* background */
  [257] = "#a2a2a2", /* foreground */
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor
 */
unsigned int defaultfg = 257;
unsigned int defaultbg = 256;
unsigned int defaultcs = 257;
unsigned int defaultrcs = 256;

/*
 * Colors used, when the specific fg == defaultfg. So in reverse mode this
 * will reverse too. Another logic would only make the simple feature too
 * complex.
 */
unsigned int defaultitalic = 7;
unsigned int defaultunderline = 7;
