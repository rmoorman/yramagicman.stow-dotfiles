/* Terminal colors (16 first used in escape sequence) */
const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#303030", /* black   */
  [1] = "#a13a38", /* red     */
  [2] = "#00715d", /* green   */
  [3] = "#006f32", /* yellow  */
  [4] = "#a43261", /* blue    */
  [5] = "#913e88", /* magenta */
  [6] = "#007086", /* cyan    */
  [7] = "#c6c6c6", /* white   */

  /* 8 bright colors */
  [8]  = "#5e5e5e", /* black   */
  [9]  = "#ffa59a", /* red     */
  [10] = "#24dfc4", /* green   */
  [11] = "#73db95", /* yellow  */
  [12] = "#ff9fc9", /* blue    */
  [13] = "#ffa7f6", /* magenta */
  [14] = "#00ddf4", /* cyan    */
  [15] = "#ffffff", /* white   */

  /* special colors */
  [256] = "#000000", /* background */
  [257] = "#919191", /* foreground */
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
