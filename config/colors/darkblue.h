/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#113457", /* black   */
  [1] = "#7f0000", /* red     */
  [2] = "#409444", /* green   */
  [3] = "#bdab00", /* yellow  */
  [4] = "#246ca7", /* blue    */
  [5] = "#6e3e7e", /* magenta */
  [6] = "#0f9595", /* cyan    */
  [7] = "#696969", /* white   */

  /* 8 bright colors */
  [8]  = "#114780", /* black   */
  [9]  = "#cd3737", /* red     */
  [10] = "#51bb56", /* green   */
  [11] = "#e1cc00", /* yellow  */
  [12] = "#56a1df", /* blue    */
  [13] = "#b374c5", /* magenta */
  [14] = "#51b2b2", /* cyan    */
  [15] = "#b7bab8", /* white   */

  /* special colors */
  [256] = "#001428", /* background */
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
