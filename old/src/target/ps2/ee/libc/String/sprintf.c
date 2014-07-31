#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Taken from somewhere on the net, dunno the license but it should not be present in a final build anyway.
// Here to make the sn tcpstack happy.

typedef short value_t;
#define HEX_CVT_MASK 0x0fff
typedef int vavalue_t;

static char* hex_convert(char* buf, value_t value)
{
  char num[32];
  int pos;
  
  *buf++ = '0';
  *buf++ = 'x';

  pos = 0;
  while (value != 0)
    {
      char c = value & 0x0F;
      num[pos++] = "0123456789ABCDEF"[(unsigned) c];
      value = (value >> 4) & HEX_CVT_MASK;
    }
  if (pos == 0)
    num[pos++] = '0';

  while (--pos >= 0)
    *buf++ = num[pos];
  
  *buf = 0;
  return buf;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static char* dec_convert(char* buf, value_t value)
{
  char num[20];
  int pos;

  pos = 0;
  if (value < 0)
    {
      *buf++ = '-';
      value = -value;
    }
  while (value != 0)
    {
      char c = value % 10;
      value = value / 10;
      num[pos++] = c + '0';
    }
  if (pos == 0)
    num[pos++] = '0';

  while (--pos >= 0)
    {
      *buf = num[pos];
      buf++;
    }
  *buf = 0;
  return buf;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Note: supports only %d, %o, %x, %d

int sprintf(char* buf, const char* pattern, ...)
{
  va_list argp;
  char* p = buf;
  char c;
  
  va_start (argp, pattern);
  while ((c = *pattern++) != 0)
    {
      if (c != '%')
        {
          *p++ = c;
        }
      else
        {
          value_t v;

          c = *pattern++;
          if (c == 'l')
            c = *pattern++;
          
          switch (c)
            {
            case 'b':
            case 'o':
            case 'x':
              v = va_arg (argp, vavalue_t);
              p = hex_convert (p, v);
              break;

            case 'd':
              v = va_arg (argp, vavalue_t);
              p = dec_convert (p, v);
              break;

            default:
              *p++ = '%';
              *p++ = c;
              break;
            }
        }
    }
  va_end (argp);
  *p++ = 0;
  return (int) (p - buf);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


