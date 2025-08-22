#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* Begin vuln global decls */


/* End vuln global decls */

int main() {
  int *p = NULL;
  unsigned long int location = 24;
  unsigned long int end = 1;
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
          location = 7;
        } else if(direction[0] == 114) {
          location = 27;
        } else if(direction[0] == 117) {
          location = 29;
        }
        break;
      case 3:
        if(direction[0] == 108) {
          location = 16;
        } else if(direction[0] == 114) {
          location = 23;
        }
        break;
      case 4:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 18;
        } else if(direction[0] == 117) {
          location = 20;
        } else if(direction[0] == 100) {
          location = 21;
        }
        break;
      case 5:
        if(direction[0] == 108) {
          location = 4;
        } else if(direction[0] == 114) {
          location = 14;
        } else if(direction[0] == 117) {
          location = 19;
        } else if(direction[0] == 100) {
          location = 23;
        }
        break;
      case 6:
        if(direction[0] == 108) {
          location = 1;
        } else if(direction[0] == 114) {
          location = 13;
        } else if(direction[0] == 117) {
          location = 17;
        } else if(direction[0] == 100) {
          location = 22;
        }
        break;
      case 7:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 8;
        } else if(direction[0] == 117) {
          location = 9;
        }
        break;
      case 8:
        if(direction[0] == 108) {
          location = 1;
        } else if(direction[0] == 114) {
          location = 7;
        } else if(direction[0] == 117) {
          location = 15;
        } else if(direction[0] == 100) {
          location = 16;
        }
        break;
      case 9:
        if(direction[0] == 108) {
          location = 7;
        } else if(direction[0] == 114) {
          location = 11;
        } else if(direction[0] == 117) {
          location = 19;
        }
        break;
      case 10:
        if(direction[0] == 108) {
          location = 1;
        } else if(direction[0] == 114) {
          location = 13;
        } else if(direction[0] == 117) {
          location = 24;
        }
        break;
      case 11:
        if(direction[0] == 108) {
          location = 0;
        } else if(direction[0] == 114) {
          location = 9;
        } else if(direction[0] == 117) {
          location = 29;
        }
        break;
      case 12:
        if(direction[0] == 108) {
          location = 16;
        } else if(direction[0] == 114) {
          location = 17;
        } else if(direction[0] == 117) {
          location = 28;
        }
        break;
      case 13:
        if(direction[0] == 108) {
          location = 6;
        } else if(direction[0] == 114) {
          location = 10;
        } else if(direction[0] == 117) {
          location = 18;
        } else if(direction[0] == 100) {
          location = 25;
        }
        break;
      case 14:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 20;
        } else if(direction[0] == 117) {
          location = 21;
        }
        break;
      case 15:
        if(direction[0] == 108) {
          location = 8;
        } else if(direction[0] == 114) {
          location = 22;
        } else if(direction[0] == 117) {
          location = 23;
        }
        break;
      case 16:
        if(direction[0] == 108) {
          location = 3;
        } else if(direction[0] == 114) {
          location = 8;
        } else if(direction[0] == 117) {
          location = 12;
        }
        break;
      case 17:
        if(direction[0] == 108) {
          location = 6;
        } else if(direction[0] == 114) {
          location = 12;
        } else if(direction[0] == 117) {
          location = 26;
        }
        break;
      case 18:
        if(direction[0] == 108) {
          location = 4;
        } else if(direction[0] == 114) {
          location = 13;
        } else if(direction[0] == 117) {
          location = 28;
        } else if(direction[0] == 100) {
          location = 29;
        }
        break;
      case 19:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 9;
        } else if(direction[0] == 117) {
          location = 25;
        } else if(direction[0] == 100) {
          location = 28;
        }
        break;
      case 20:
        if(direction[0] == 108) {
          location = 4;
        } else if(direction[0] == 114) {
          location = 14;
        } else if(direction[0] == 117) {
          location = 26;
        } else if(direction[0] == 100) {
          location = 27;
        }
        break;
      case 21:
        if(direction[0] == 108) {
          location = 4;
        } else if(direction[0] == 114) {
          location = 14;
        } else if(direction[0] == 117) {
          location = 24;
        }
        break;
      case 22:
        if(direction[0] == 108) {
          location = 1;
        } else if(direction[0] == 114) {
          location = 6;
        } else if(direction[0] == 117) {
          location = 15;
        } else if(direction[0] == 100) {
          location = 23;
        }
        break;
      case 23:
        if(direction[0] == 108) {
          location = 3;
        } else if(direction[0] == 114) {
          location = 5;
        } else if(direction[0] == 117) {
          location = 15;
        } else if(direction[0] == 100) {
          location = 22;
        }
        break;
      case 24:
        if(direction[0] == 108) {
          location = 10;
        } else if(direction[0] == 114) {
          location = 21;
        } else if(direction[0] == 117) {
          location = 25;
        }
        break;
      case 25:
        if(direction[0] == 108) {
          location = 13;
        } else if(direction[0] == 114) {
          location = 19;
        } else if(direction[0] == 117) {
          location = 24;
        }
        break;
      case 26:
        if(direction[0] == 108) {
          location = 17;
        } else if(direction[0] == 114) {
          location = 20;
        }
        break;
      case 27:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 20;
        }
        break;
      case 28:
        if(direction[0] == 108) {
          location = 12;
        } else if(direction[0] == 114) {
          location = 18;
        } else if(direction[0] == 117) {
          location = 19;
        }
        break;
      case 29:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 11;
        } else if(direction[0] == 117) {
          location = 18;
        }
        break;
      case 0:
        while (y > 1) {
          if (y % 2 == 1) {
            y = 3 * y + 1;
          } else {
            y = y / 2;
          }
        }
        if ((path - y) > 215 && (path + y) < 219) {
          #ifdef PATCHED
            y++;
          #endif
          #ifndef PATCHED
printf("Crash\n");
 y=5/(y-y);
          #endif
        }
        if(direction[0] == 108) {
          location = 11;
        }
        break;
      case 1:
        if(direction[0] == 108) {
          location = 6;
        } else if(direction[0] == 114) {
          location = 8;
        } else if(direction[0] == 117) {
          location = 10;
        } else if(direction[0] == 100) {
          location = 22;
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
