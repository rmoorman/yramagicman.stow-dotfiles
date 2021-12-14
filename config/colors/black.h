/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#686868", /* black   */
  [1] = "#787878", /* red     */
  [2] = "#9c9c9c", /* green   */
  [3] = "#a7a7a7", /* yellow  */
  [4] = "#c0c0c0", /* blue    */
  [5] = "#c6c6c6", /* magenta */
  [6] = "#dfdfdf", /* cyan    */
  [7] = "#ededed", /* white   */

  /* 8 bright colors */
  [8]  = "#aaaaaa", /* black   */
  [9]  = "#9d9d9d", /* red     */
  [10] = "#c6c6c6", /* green   */
  [11] = "#c1c1c1", /* yellow  */
  [12] = "#d7d7d7", /* blue    */
  [13] = "#e2e2e2", /* magenta */
  [14] = "#eaeaea", /* cyan    */
  [15] = "#ffffff", /* white   */

  /* special colors */
  [256] = "#000000", /* background */
  [257] = "#d8d8d8", /* foreground */
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
