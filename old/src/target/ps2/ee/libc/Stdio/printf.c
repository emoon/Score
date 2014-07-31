#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(PS2_SCE)
void kputs(const char* s);
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static char* itoaBuffer(int value, char* buf)
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


static char* ftoaBuffer(float n, char* a) 
{
	unsigned int i=0,j=0,k=0;
	int x,y,t;

	if (n<0) 
	{
		a[j++] = '-';
		n = -n;
	}

	y = x = (int)floor(n);

	if(x == 0) 
		a[j++] = 48;

	k = j;

	while(x>0) 
	{
		t = x%10;
		x/=10;
		a[k++]=t+48;
	}
	
	i = k--;
	while(k>j) 
	{
		char temp = a[j];
		a[j] = a[k];
		a[k] = temp;
		j++; 
		k--;
	}
	j = i;
	n = n-y;
	a[j++] = '.';
	
	for(k=0;k<4;k++) 
	{
		n = n*10;
		t = (int)floor(n)%10;
		a[j++] = t+48;
	}

	a[j] = 0;

	return a+j;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static char* hex_convert(char* buf, unsigned int value)
{
  char num[32];
  int pos;
  
  *buf++ = '0';
  *buf++ = 'x';

  pos = 0;
  while (value != 0)
    {
      char c = value & 0x0F;
      num[pos++] = "0123456789ABCDEF"[(unsigned int)c];
      value = (value >> 4);
    }
  if (pos == 0)
    num[pos++] = '0';

  while (--pos >= 0)
    *buf++ = num[pos];
  
  *buf = 0;
  return buf;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static int simpleSprintf(char* buf, const char* pattern, va_list args)
{
	char* p = buf;
	char c;

	while ((c = *pattern++) != 0)
	{
		if (c != '%')
		{
			*p++ = c;
		}
		else
		{
			int iv;
			double dv;
		
			c = *pattern++;
			if (c == 'l')
				c = *pattern++;
	
			switch (c)
			{
				case 'b':
				case 'o':
				case 'x':
					iv = va_arg(args, int);
					p = hex_convert (p, iv);
					break;

				case 'd':
					iv = va_arg(args, int);
					p = itoaBuffer(iv, p);
					break;

				case 'f':
					dv = va_arg(args, double);
					p = ftoaBuffer((float)dv, p);
					break;

				default:
					*p++ = '%';
					*p++ = c;
					break;
			}
		}
	}
	
	*p++ = 0;
	return (int) (p - buf);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int vsnprintf(char* str, unsigned int size, const char* format, va_list args)
{
	return simpleSprintf(str, format, args);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void flush_stream(const char* buffer)
{
	const char* begin = buffer;
	const char* end = begin + strlen(begin);

#if defined(PS2_SCE)
	for (; begin != end;)
	{
		char buffer[115];
		int length = (end - begin) < (sizeof(buffer)-1) ? (end - begin) : (sizeof(buffer)-1);
		memcpy(buffer, begin, length);
		buffer[length] = '\0';
		kputs(buffer);
		begin += length;
	}
#else
	fioWrite(1, begin, end - begin);
#endif
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int printf(const char* format, ...)
{
	char dest[1024];

	va_list list;
	va_start(list, format);
	int result = simpleSprintf(dest, format, list);
	va_end(list);

#if defined(PS2_SCE)
	flush_stream(dest);
#endif

	return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int puts(const char* string)
{
	return printf("%s\n", string);
}


