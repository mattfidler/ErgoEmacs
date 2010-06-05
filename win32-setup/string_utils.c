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

#include <assert.h>
#include <string.h>
#include <stdlib.h>

typedef struct String
{
  char* text;
  int length;
  int size;
} String;

String* string_create(const char* text)
{
  int text_length;
  String *s;

  assert(text != NULL);

  s = malloc(sizeof(String));
  if (s == NULL)
    return NULL;

  text_length = strlen(text);

  s->size = text_length+1;
  s->length = text_length;
  s->text = malloc(sizeof(char) * s->size);

  memcpy(s->text,
	 text,
	 text_length+1);

  return s;
}

void string_destroy(String* s)
{
  assert(s != NULL);
  assert(s->text != NULL);

  free(s->text);
  free(s);
}

void string_reserve(String* s, int text_length)
{
  assert(s != NULL);
  assert(s->text != NULL);

  if (text_length >= s->size) {
    s->size += text_length + 256;
    s->text = realloc(s->text, s->size);
  }

  assert(s->size > text_length);
}

void string_append(String* s, const char* text)
{
  int text_length;
  assert(text != NULL);
  text_length = strlen(text);

  if (text_length == 0)
    return;

  string_reserve(s, s->length + text_length);

  memcpy(s->text+s->length,
	 text,
	 text_length+1);

  s->length += text_length;

  assert(strlen(s->text) == s->length);
}

void string_prepend(String* s, const char* text)
{
  int text_length;
  assert(text != NULL);
  text_length = strlen(text);

  if (text_length == 0)
    return;

  string_reserve(s, s->length + text_length);

  memmove(s->text + text_length,
  	  s->text,
  	  s->length + 1);

  memcpy(s->text,
	 text,
	 text_length);

  s->length += text_length;

  assert(strlen(s->text) == s->length);
}

#ifdef TEST
#include <stdio.h>

static void test1()
{
  String* a = string_create("");
  String* b = string_create("Hello");
  String* c = string_create("World");

  printf("a = \"%s\"\n", a->text);
  printf("b = \"%s\"\n", b->text);
  printf("c = \"%s\"\n", c->text);
  assert(strcmp(a->text, "") == 0);
  assert(strcmp(b->text, "Hello") == 0);
  assert(strcmp(c->text, "World") == 0);

  string_append(a, b->text);
  string_append(b, c->text);
  string_append(c, a->text);

  printf("a = a+b = \"%s\"\n", a->text);
  printf("b = b+c = \"%s\"\n", b->text);
  printf("c = c+a = \"%s\"\n", c->text);
  assert(strcmp(a->text, "Hello") == 0);
  assert(strcmp(b->text, "HelloWorld") == 0);
  assert(strcmp(c->text, "WorldHello") == 0);

  string_prepend(a, b->text);
  string_prepend(b, c->text);
  string_prepend(c, a->text);

  printf("a = b+a = \"%s\"\n", a->text);
  printf("b = c+b = \"%s\"\n", b->text);
  printf("c = a+c = \"%s\"\n", c->text);
  assert(strcmp(a->text, "HelloWorldHello") == 0);
  assert(strcmp(b->text, "WorldHelloHelloWorld") == 0);
  assert(strcmp(c->text, "HelloWorldHelloWorldHello") == 0);

  string_destroy(a);
  string_destroy(b);
  string_destroy(c);
}

static void test2()
{
  String* a = string_create("");
  int i;

  for (i=1; i<=10000; ++i) {
    string_append(a, "a");

    assert(a->length == i);
    assert(strlen(a->text) == i);
  }

  for (i=0; i<10000; ++i)
    assert(a->text[i] == 'a');
  assert(a->text[i] == '\0');
    
  string_destroy(a);
}

int main()
{
  test1();
  test2();
  return 0;
}

#endif
