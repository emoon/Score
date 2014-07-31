int strlen(const char* s)
{
	const char* c;
	for (c = s; *c; ++c)
		continue;
	return (c-s);
}
