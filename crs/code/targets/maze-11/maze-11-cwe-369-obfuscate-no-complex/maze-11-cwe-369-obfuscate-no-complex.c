#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* Begin vuln global decls */


/* End vuln global decls */

int main() {
  int *p = NULL;
  unsigned long int location = 2;
  unsigned long int end = 4;
  char direction[2];
  int ret;
  size_t size;
  long int path = location;
  const char* welcome = "Welcome to Falken's Maze. Commands are r, l, u, d, followed by <ENTER>. Play until you win!\n";
  const char* win_string = "You won!\n";
  /* Begin vuln decls */


  /* End vuln decls */

  printf(welcome);

  while(1) {
    ret=scanf("%1s",direction);
    if (ret<0) {printf("Bye\n");exit(0);}
    long int y = path + direction[0];

    printf("You went %s\n", direction);

    switch (location) {
      case 2:
        if(direction[0] == 108) {
          location = 3;
        } else if(direction[0] == 114) {
          location = 6;
        } else if(direction[0] == 117) {
          location = 10;
        }
        break;
      case 3:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 8;
        }
        break;
      case 4:
        if(direction[0] == 108) {
          location = 6;
        }
        break;
      case 5:
        if(direction[0] == 108) {
          location = 0;
        } else if(direction[0] == 114) {
          location = 1;
        } else if(direction[0] == 117) {
          location = 9;
        }
        break;
      case 6:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 4;
        }
        break;
      case 7:
        while (y > 1) {
          if (y % 2 == 1) {
            y = 3 * y + 1;
          } else {
            y = y / 2;
          }
        }
        if ((path - y) > 32 && (path + y) < 36) {
          #ifdef PATCHED
            y++;
          #endif
          #ifndef PATCHED
printf("Crash\n");
 y=5/(y-y);
          #endif
        }
        if(direction[0] == 108) {
          location = 0;
        }
        break;
      case 8:
        if(direction[0] == 108) {
          location = 3;
        } else if(direction[0] == 114) {
          location = 9;
        }
        break;
      case 9:
        if(direction[0] == 108) {
          location = 1;
        } else if(direction[0] == 114) {
          location = 5;
        } else if(direction[0] == 117) {
          location = 8;
        }
        break;
      case 10:
        if(direction[0] == 108) {
          location = 2;
        }
        break;
      case 0:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 7;
        }
        break;
      case 1:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 9;
        }
        break;

    }
    if(location == end) {
      printf(win_string);
      break;
    }
    path += location;
  }
}
