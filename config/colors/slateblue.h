/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#000000", /* black   */
  [1] = "#ba3030", /* red     */
  [2] = "#2da133", /* green   */
  [3] = "#8d840c", /* yellow  */
  [4] = "#4668b9", /* blue    */
  [5] = "#6e3e7e", /* magenta */
  [6] = "#0c7b7b", /* cyan    */
  [7] = "#979797", /* white   */

  /* 8 bright colors */
  [8]  = "#9c9c9c", /* black   */
  [9]  = "#ba6e6e", /* red     */
  [10] = "#65c369", /* green   */
  [11] = "#b6ab18", /* yellow  */
  [12] = "#5984eb", /* blue    */
  [13] = "#b669cc", /* magenta */
  [14] = "#51b2b2", /* cyan    */
  [15] = "#d7d7d7", /* white   */

  /* special colors */
  [256] = "#2d4257", /* background */
  [257] = "#c5c8c6", /* foreground */
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor
 */
static unsigned int defaultfg = 257;
static unsigned int defaultbg = 256;
static unsigned int defaultcs = 257;

/*
 * Colors used, when the specific fg == defaultfg. So in reverse mode this
 * will reverse too. Another logic would only make the simple feature too
 * complex.
 */
static unsigned int defaultitalic = 7;
static unsigned int defaultunderline = 7;
