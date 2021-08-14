/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#1b181b", /* black   */
  [1] = "#ca402b", /* red     */
  [2] = "#379a37", /* green   */
  [3] = "#bb8a35", /* yellow  */
  [4] = "#516aec", /* blue    */
  [5] = "#7b59c0", /* magenta */
  [6] = "#159393", /* cyan    */
  [7] = "#ab9bab", /* white   */

  /* 8 bright colors */
  [8]  = "#776977", /* black   */
  [9]  = "#ca402b", /* red     */
  [10] = "#379a37", /* green   */
  [11] = "#bb8a35", /* yellow  */
  [12] = "#516aec", /* blue    */
  [13] = "#7b59c0", /* magenta */
  [14] = "#159393", /* cyan    */
  [15] = "#f7f3f7", /* white   */

  /* special colors */
  [256] = "#f7f3f7", /* background */
  [257] = "#695d69", /* foreground */
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
