/* Some libc's (such as musl) define the following as macros pointing to the non-64 versions: */
#ifdef open64
#undef open64
#endif
#ifdef tmpfile64
#undef tmpfile64
#endif
#ifdef fopen64
#undef fopen64
#endif
#ifdef freopen64
#undef freopen64
#endif
#ifdef fseeko64
#undef fseeko64
#endif
#ifdef ftello64
#undef ftello64
#endif
#ifdef fgetpos64
#undef fgetpos64
#endif
#ifdef fsetpos64
#undef fsetpos64
#endif
#ifdef fpos64
#undef fpos64
#endif
#ifdef off64
#undef off64
#endif
