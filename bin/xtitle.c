/* xtitle.c - generate escape codes to set xterm/rxvt titles */

#include <stdio.h>
#include <string.h>

int
main(int argc, char **argv)
{
  char *prog_name;
  char *args;
  int length;
  int arg;

  prog_name = strrchr(argv[0], '/');
  if (prog_name == NULL)
    prog_name = argv[0];
  else
    prog_name++;
  for (length = 0, arg = 1; arg < argc; length += strlen(argv[arg]), arg++)
    ;
  args = (char *)malloc(length + arg + 5);
  if (args == NULL) {
    fprintf(stderr, "%s: could not allocate %d bytes for string\n", prog_name,
	    length + arg);
    exit(1);
  }
  *args = '\0';
  if (strcasecmp(prog_name, "xtitle") == 0)
    strcpy(args, "]2;");
  else if (strcasecmp(prog_name, "xicontitle") == 0)
    strcpy(args, "]1;");
  else
    strcpy(args, "]0;");
  for (arg = 1; arg < argc; arg++) {
    strcat(args, argv[arg]);
    if ((arg + 1) < argc)
      strcat(args, " ");
  }
  strcat(args, "");
  (void) write(1, args, length + arg + 4); /* don't write trailing \0 */
  free(args);
  exit(0);
}
