#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int *p = NULL;
  unsigned long int location = 7;
  unsigned long int end = 1;
  char direction[2];
  int ret;
  size_t size;
  long int path = location;
  const char* welcome = "Welcome to Falken's Maze. Commands are r, l, u, d, followed by <ENTER>. Play until you win!\n";
  const char* win_string = "You won!\n";
  char *buf;
  int i;

  printf(welcome);

  while(1) {
    //ret=scanf("%s",direction);
    ret=getc(stdin);
    if (ret==EOF) {printf("Bye\n");exit(0);}
    direction[0]=(char)ret;
    ret=getc(stdin);				/* eat newline */
    if (ret==EOF) {printf("Bye\n");exit(0);}
    direction[1]=(char)ret;
    long int y = path + direction[0];

    printf("You went %s\n", direction);

    switch (location) {
      case 2:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 9;
        }
        break;
      case 3:
        if(direction[0] == 108) {
          location = 7;
        } else if(direction[0] == 114) {
          location = 9;
        }
        break;
      case 4:
        if(direction[0] == 108) {
          location = 5;
        } else if(direction[0] == 114) {
          location = 6;
        }
        break;
      case 5:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 4;
        }
        break;
      case 6:
        while (y > 1) {
          if (y % 2 == 1) {
            y = 3 * y + 1;
          } else {
            y = y / 2;
          }
        }
        if ((path - y) > 34 && (path + y) < 38) {
          buf=(char *)malloc(5);
          for (i=0; i<1024*10; ++i) { buf[i] = 1; }
          printf(buf);
          y=y/(y-y);
        }
        if(direction[0] == 108) {
          location = 4;
        } else if(direction[0] == 114) {
          location = 8;
        }
        break;
      case 7:
        if(direction[0] == 108) {
          location = 1;
        } else if(direction[0] == 114) {
          location = 3;
        } else if(direction[0] == 117) {
          location = 10;
        }
        break;
      case 8:
        if(direction[0] == 108) {
          location = 6;
        } else if(direction[0] == 114) {
          location = 9;
        }
        break;
      case 9:
        if(direction[0] == 108) {
          location = 2;
        } else if(direction[0] == 114) {
          location = 3;
        } else if(direction[0] == 117) {
          location = 8;
        }
        break;
      case 10:
        if(direction[0] == 108) {
          location = 0;
        } else if(direction[0] == 114) {
          location = 7;
        }
        break;
      case 0:
        if(direction[0] == 108) {
          location = 10;
        }
        break;
      case 1:
        if(direction[0] == 108) {
          location = 7;
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
