/* Copyright (C) 2010 David Capello
   Developed for ErgoEmacs

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
*/

#ifndef STRING_UTILS_H
#define STRING_UTILS_H

typedef struct String
{
  char* text;
  int length;
  int size;
} String;

String* string_create  (const char* text);
void    string_destroy (String* s);

void    string_reserve (String* s, int text_length);
void    string_append  (String* s, const char* text);
void    string_prepend (String* s, const char* text);

#endif

