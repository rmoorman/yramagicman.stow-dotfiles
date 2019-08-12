/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#0a0a0a", /* black   */
  [1] = "#dbdbdb", /* red     */
  [2] = "#a3a3a3", /* green   */
  [3] = "#fcfcfc", /* yellow  */
  [4] = "#e3e3e3", /* blue    */
  [5] = "#a1a1a1", /* magenta */
  [6] = "#f5f5f5", /* cyan    */
  [7] = "#a6a6a6", /* white   */

  /* 8 bright colors */
  [8]  = "#5c5c5c", /* black   */
  [9]  = "#dbdbdb", /* red     */
  [10] = "#a3a3a3", /* green   */
  [11] = "#fcfcfc", /* yellow  */
  [12] = "#e3e3e3", /* blue    */
  [13] = "#a1a1a1", /* magenta */
  [14] = "#f5f5f5", /* cyan    */
  [15] = "#f7f7f7", /* white   */

  /* special colors */
  [256] = "#0a0a0a", /* background */
  [257] = "#a6a6a6", /* foreground */
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor
 */
unsigned int defaultfg = 257;
unsigned int defaultbg = 256;
unsigned int defaultcs = 257;
unsigned int defaultrcs = 257;

/*
 * Colors used, when the specific fg == defaultfg. So in reverse mode this
 * will reverse too. Another logic would only make the simple feature too
 * complex.
 */
static unsigned int defaultitalic = 7;
static unsigned int defaultunderline = 7;
