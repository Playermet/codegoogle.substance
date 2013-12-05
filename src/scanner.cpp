/**************************************************************************
 * Language scanner
 *
 * Copyright (c) 2013 Randy Hollines
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the distribution.
 * - Neither the name of the Substance team nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ***************************************************************************/

#include "scanner.h"

using namespace compiler;

#define EOB L'\0'

/****************************
 * Scanner constructor
 ****************************/
Scanner::Scanner(const wstring &input, bool is_file)
{
  // create tokens
  for(int i = 0; i < LOOK_AHEAD; i++) {
    tokens[i] = new Token;
  }
  // load identifiers into map
  LoadKeywords();
	
	if(is_file) {
		file_name = input;		
		ReadFile(input);
	}
	else {
		ReadLine(input);
	}
	line_num = 1;
}

/****************************
 * Scanner destructor
 ****************************/
Scanner::~Scanner()
{
  // delete buffer
  if(buffer) {
    delete[] buffer;
    buffer = NULL;
  }

  for(int i = 0; i < LOOK_AHEAD; i++) {
    Token* temp = tokens[i];
    delete temp;
    temp = NULL;
  }
}

/****************************
 * Loads language keywords
 ****************************/
void Scanner::LoadKeywords()
{
  ident_map[L"New"] = TOKEN_NEW_ID;
	ident_map[L"var"] = TOKEN_VAR_ID;
  ident_map[L"if"] = TOKEN_IF_ID;
  ident_map[L"else"] = TOKEN_ELSE_ID;
	ident_map[L"while"] = TOKEN_WHILE_ID;
  ident_map[L"dump"] = TOKEN_DUMP_ID;
  ident_map[L"self"] = TOKEN_SELF_ID;
  ident_map[L"class"] = TOKEN_CLASS_ID;
  ident_map[L"return"] = TOKEN_RETURN_ID;
  ident_map[L"class"] = TOKEN_CLASS_ID;
  ident_map[L"vars"] = TOKEN_VARS_ID;	
  ident_map[L"func"] = TOKEN_FUNC_ID;
  ident_map[L"true"] = TOKEN_TRUE_LIT;
  ident_map[L"false"] = TOKEN_FALSE_LIT;
}

/****************************
 * Processes language
 * identifies
 ****************************/
void Scanner::CheckIdentifier(int index)
{
  // copy wstring
  const int length = end_pos - start_pos;
  wstring ident(buffer, start_pos, length);
  // check wstring
  enum ScannerTokenType ident_type = ident_map[ident];
  switch(ident_type) {
  case TOKEN_NEW_ID:
  case TOKEN_TRUE_LIT:
  case TOKEN_FALSE_LIT:
  case TOKEN_CLASS_ID:
  case TOKEN_FUNC_ID:
	case TOKEN_VAR_ID:
  case TOKEN_VARS_ID:
	case TOKEN_IF_ID:
  case TOKEN_ELSE_ID:
	case TOKEN_WHILE_ID:
  case TOKEN_DUMP_ID:
  case TOKEN_SELF_ID:
  case TOKEN_RETURN_ID:
    tokens[index]->SetType(ident_type);
		tokens[index]->SetLineNbr(line_num);
		tokens[index]->SetFileName(file_name);
    break;
		
  default:
    tokens[index]->SetType(TOKEN_IDENT);
		tokens[index]->SetLineNbr(line_num);
		tokens[index]->SetFileName(file_name);
    tokens[index]->SetIdentifier(ident);
    break;
  }
}

/****************************
 * Reads a source input file
 ****************************/
void Scanner::ReadLine(const wstring &line)
{
  buffer_pos = 0;
  buffer = new wchar_t[line.size() + 1];
#ifdef _WIN32
  wcsncpy_s(buffer, line.size(), line.c_str(), _TRUNCATE);
#else
  wcsncpy(buffer, line.c_str(), line.size());
#endif
  buffer[line.size()] = '\0';
  buffer_size = line.size() + 1;
#ifdef _DEBUG
  wcout << L"---------- Source ---------" << endl;
  wcout << buffer << endl;
#endif
}

/****************************
 * Reads a source input file
 ****************************/
void Scanner::ReadFile(const wstring &name)
{
  buffer_pos = 0;
  buffer = LoadFileBuffer(name, buffer_size);
  
#ifdef _DEBUG
  wcout << L"---------- Source ---------" << endl;
  wcout << buffer << endl;
#endif
}

/****************************
 * Processes the next token
 ****************************/
void Scanner::NextToken()
{
  if(buffer_pos == 0) {
    NextChar();
    for(int i = 0; i < LOOK_AHEAD; i++) {
      ParseToken(i);
    }
  } else {
    int i = 1;
    for(; i < LOOK_AHEAD; i++) {
      tokens[i - 1]->Copy(tokens[i]);
    }
    ParseToken(i - 1);
  }
}

/****************************
 * Gets the current token
 ****************************/
Token* Scanner::GetToken(int index)
{
  if(index < LOOK_AHEAD) {
    return tokens[index];
  }

  return NULL;
}

/****************************
 * Gets the next character.
 * Note, EOB is returned at
 * end of a stream
 ****************************/
void Scanner::NextChar()
{
	if(buffer_pos < buffer_size) {
    // line number
    if(cur_char == L'\n') {
      line_num++;
    }
    // current character    
    cur_char = buffer[buffer_pos++];
    // next character
    if(buffer_pos < buffer_size) {
      nxt_char = buffer[buffer_pos];
      // next next character
      if(buffer_pos + 1 < buffer_size) {
        nxt_nxt_char = buffer[buffer_pos + 1];
      }
      // end of file
      else {
        nxt_nxt_char = EOB;
      }
    }
    // end of file
    else {
      nxt_char = EOB;
    }
  }
  // end of file
  else {
    cur_char = EOB;
  }
}

/****************************
 * Processes white space
 ****************************/
void Scanner::Whitespace()
{
  while(WHITE_SPACE && cur_char != EOB) {

    NextChar();
  }
}

/****************************
 * Parses a token
 ****************************/
void Scanner::ParseToken(int index)
{
  // unable to load buffer
  if(!buffer) {
    tokens[index]->SetType(TOKEN_NO_INPUT);
		tokens[index]->SetLineNbr(line_num);
		tokens[index]->SetFileName(file_name);
    return;
  }
  // ignore white space
  Whitespace();
  // ignore comments
  while(cur_char == COMMENT && cur_char != EOB) {
    NextChar();
    // extended comment
    if(cur_char == EXTENDED_COMMENT) {
      NextChar();
      while(!(cur_char == EXTENDED_COMMENT && nxt_char == COMMENT) && cur_char != EOB) {
        NextChar();
      }
      NextChar();
      NextChar();
    }
    // line comment
    else {
      while(cur_char != L'\n' && cur_char != EOB) {
        NextChar();
      }
    }
    Whitespace();
  }
  // character wstring
  if(cur_char == L'\"') {
    NextChar();
    // mark
    start_pos = (int)buffer_pos - 1;
    while(cur_char != L'\"' && cur_char != EOB) {
      if(cur_char == L'\\') {
        NextChar();
        switch(cur_char) {
        case L'"':
          break;

        case L'\\':
          break;

        case L'n':
          break;

        case L'r':
          break;

        case L't':
          break;

        case L'0':
          break;

        default:
          tokens[index]->SetType(TOKEN_UNKNOWN);
					tokens[index]->SetLineNbr(line_num);
					tokens[index]->SetFileName(file_name);
          NextChar();
          break;
        }
      }
      NextChar();
    }
    // mark
    end_pos = (int)buffer_pos - 1;
    // check wstring
    NextChar();
    CheckString(index);
    return;
  }
  // character
  else if(cur_char == L'\'') {
		NextChar();
    // escape or hex/unicode encoding
    if(cur_char == L'\\') {
      NextChar();
      // read unicode string
      if(cur_char == L'u') {
        NextChar();
        start_pos = (int)buffer_pos - 1;
        while(iswdigit(cur_char) || (cur_char >= L'a' && cur_char <= L'f') ||
							(cur_char >= L'A' && cur_char <= L'F')) {
					NextChar();
        }
        end_pos = (int)buffer_pos - 1;
        ParseUnicodeChar(index);
        if(cur_char != L'\'') {
          tokens[index]->SetType(TOKEN_UNKNOWN);
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
        }
        NextChar();
        return;
      }
      // escape
      else if(nxt_char == L'\'') {
        switch(cur_char) {
        case L'n':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\n');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L'r':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\r');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L't':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\t');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L'a':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\a');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L'b':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\b');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L'f':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\f');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;
					
        case L'\\':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\\');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L'\'':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\'');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;

        case L'0':
          tokens[index]->SetType(TOKEN_CHAR_LIT);
          tokens[index]->SetCharLit(L'\0');
          tokens[index]->SetLineNbr(line_num);
          tokens[index]->SetFileName(file_name);
          NextChar();
          NextChar();
          return;
        }
      }
      // error
      else {
        tokens[index]->SetType(TOKEN_UNKNOWN);
        tokens[index]->SetLineNbr(line_num);
        tokens[index]->SetFileName(file_name);
        NextChar();
        return;
      }
    } 
		// error
		else {
      if(nxt_char != L'\'') {
        tokens[index]->SetType(TOKEN_UNKNOWN);
        tokens[index]->SetLineNbr(line_num);
        tokens[index]->SetFileName(file_name);
        NextChar();
        return;
      } 
			else {
        tokens[index]->SetType(TOKEN_CHAR_LIT);
        tokens[index]->SetCharLit(cur_char);
        tokens[index]->SetLineNbr(line_num);
        tokens[index]->SetFileName(file_name);
        NextChar();
        NextChar();
        return;
      }
    }
	}
  // identifier
  else if(isalpha(cur_char) || cur_char == L'@' || cur_char == L'?') {
    // mark
    start_pos = (int)buffer_pos - 1;
    while((isalpha(cur_char) || isdigit(cur_char) || cur_char == L'_' || 
					 cur_char == L'@' || cur_char == L'?' || cur_char == L'.') && cur_char != EOB) {
      NextChar();
    }
    // mark
    end_pos = (int)buffer_pos - 1;
    // check identifier
    CheckIdentifier(index);
    return;
  }
  // number
  else if(iswdigit(cur_char) || (cur_char == L'.' && iswdigit(nxt_char))) {
    bool is_double = false;
    int hex_state = 0;
    // mark
    start_pos = (int)buffer_pos - 1;

    // test hex state
    if(cur_char == L'0') {
      hex_state = 1;
    }
    while(iswdigit(cur_char) || (cur_char == L'.' && iswdigit(nxt_char)) || cur_char == L'x' ||
					(cur_char >= L'a' && cur_char <= L'f') ||
					(cur_char >= L'A' && cur_char <= L'F')) {
      // decimal double
      if(cur_char == L'.') {
				// error
				if(is_double) {
					tokens[index]->SetType(TOKEN_UNKNOWN);
					tokens[index]->SetLineNbr(line_num);
					tokens[index]->SetFileName(file_name);
					NextChar();
					break;
				}
				is_double = true;
      }
      // hex integer
      if(cur_char == L'x') {
				if(hex_state == 1) {
					hex_state = 2;
				}
				else {
					hex_state = 1;
				}
      }
      else {
				hex_state = 0;
      }
      // next character
      NextChar();
    }
    // mark
    end_pos = (int)buffer_pos - 1;
    if(is_double) {
      ParseDouble(index);
    } 
    else if(hex_state == 2) {
      ParseInteger(index, 16);
    }
    else if(hex_state) {
      tokens[index]->SetType(TOKEN_UNKNOWN);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
    }
    else {
      ParseInteger(index);
    }
    return;
  }
  // other
  else {
    switch(cur_char) {
    case L':':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_ASSIGN);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else {
        tokens[index]->SetType(TOKEN_COLON);
			  tokens[index]->SetLineNbr(line_num);
			  tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'-':
      if(nxt_char == L'>') {
        NextChar();
        tokens[index]->SetType(TOKEN_ASSESSOR);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_SUB_EQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else {
        tokens[index]->SetType(TOKEN_SUB);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'{':
      tokens[index]->SetType(TOKEN_OPEN_BRACE);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L'}':
      tokens[index]->SetType(TOKEN_CLOSED_BRACE);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L'.':
      tokens[index]->SetType(TOKEN_PERIOD);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;
      
    case L'[':
      tokens[index]->SetType(TOKEN_OPEN_BRACKET);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L']':
      tokens[index]->SetType(TOKEN_CLOSED_BRACKET);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L'(':
      tokens[index]->SetType(TOKEN_OPEN_PAREN);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L')':
      tokens[index]->SetType(TOKEN_CLOSED_PAREN);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L',':
      tokens[index]->SetType(TOKEN_COMMA);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L';':
      tokens[index]->SetType(TOKEN_SEMI_COLON);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L'&':
      tokens[index]->SetType(TOKEN_AND);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L'|':
      tokens[index]->SetType(TOKEN_OR);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case L'=':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_EQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } else {
        tokens[index]->SetType(TOKEN_ASSIGN);
        tokens[index]->SetLineNbr(line_num);
        tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      break;
      
    case L'!':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_NEQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } else {
        tokens[index]->SetType(TOKEN_NOT);
        tokens[index]->SetLineNbr(line_num);
        tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;
      
    case L'<':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_LEQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else if(nxt_char == L'>') {
        NextChar();
        tokens[index]->SetType(TOKEN_NEQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else {
        tokens[index]->SetType(TOKEN_LES);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'>':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_GEQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else {
        tokens[index]->SetType(TOKEN_GTR);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'+':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_ADD_EQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } 
      else {
        tokens[index]->SetType(TOKEN_ADD);
			  tokens[index]->SetLineNbr(line_num);
			  tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'*':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_MUL_EQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } else {
        tokens[index]->SetType(TOKEN_MUL);
			  tokens[index]->SetLineNbr(line_num);
			  tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'/':
      if(nxt_char == L'=') {
        NextChar();
        tokens[index]->SetType(TOKEN_DIV_EQL);
				tokens[index]->SetLineNbr(line_num);
				tokens[index]->SetFileName(file_name);
        NextChar();
      } else {
        tokens[index]->SetType(TOKEN_DIV);
			  tokens[index]->SetLineNbr(line_num);
			  tokens[index]->SetFileName(file_name);
        NextChar();
      }
      break;

    case L'%':
      tokens[index]->SetType(TOKEN_MOD);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;

    case EOB:
      tokens[index]->SetType(TOKEN_END_OF_STREAM);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      break;

    default:
      ProcessWarning();
      tokens[index]->SetType(TOKEN_UNKNOWN);
			tokens[index]->SetLineNbr(line_num);
			tokens[index]->SetFileName(file_name);
      NextChar();
      break;
    }
    return;
  }
}
