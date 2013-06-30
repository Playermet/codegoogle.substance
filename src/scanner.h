/***************************************************************************
 * Language scanner
 *
 * Copyright (c) 2013 Randy Hollines
 * All rights reserved.
 *
 * Reistribution and use in source and binary forms, with or without
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

#ifndef __SCANNER_H__
#define __SCANNER_H__

#include "common.h"

using namespace std;

// comment
#define COMMENT '#'
#define EXTENDED_COMMENT '~'

// look ahead value
#define LOOK_AHEAD 3
// white space
#define WHITE_SPACE (cur_char == ' ' || cur_char == '\t' || cur_char == '\r' || cur_char == '\n')

namespace compiler {
  /****************************
   * Token types
   ****************************/
  enum ScannerTokenType {
    // misc
    TOKEN_END_OF_STREAM = -1000,
    TOKEN_NO_INPUT,
    TOKEN_UNKNOWN,
    // symbols
    TOKEN_PERIOD,
    TOKEN_COLON,
    TOKEN_SEMI_COLON,
    TOKEN_COMMA,
    TOKEN_ASSIGN,
    TOKEN_OPEN_BRACE,
    TOKEN_CLOSED_BRACE,
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSED_PAREN,
    TOKEN_OPEN_BRACKET,
    TOKEN_CLOSED_BRACKET,
    TOKEN_ASSESSOR,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_NOT,
    TOKEN_EQL,
    TOKEN_NEQL,
    TOKEN_LES,
    TOKEN_GTR,
    TOKEN_GEQL,
    TOKEN_LEQL,
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_ADD_EQL,
    TOKEN_SUB_EQL,
    TOKEN_MUL_EQL,
    TOKEN_DIV_EQL,
    // literals
    TOKEN_IDENT,
    TOKEN_TRUE_LIT,
    TOKEN_FALSE_LIT,
    TOKEN_INT_LIT,
    TOKEN_FLOAT_LIT,
    TOKEN_CHAR_LIT,
    TOKEN_CHAR_STRING_LIT,
    // reserved words
	  TOKEN_IF_ID,
    TOKEN_ELSE_ID,
	  TOKEN_WHILE_ID,
    TOKEN_DUMP_ID,
    TOKEN_THIS_ID,
    TOKEN_CLASS_ID,
    TOKEN_METHOD_ID,
  };

  /****************************
   * Token class
   ****************************/
  class Token {
    enum ScannerTokenType token_type;
    wstring ident;
	  unsigned int line_num;
    wstring file_name;
	
    INT_T int_lit;
    FLOAT_T double_lit;
    CHAR_T char_lit;
    BYTE_T byte_lit;
	
   public:
    inline void Copy(Token* token) {
		  line_num = token->GetLineNumber();
      char_lit = token->GetCharLit();
      int_lit = token->GetIntLit();
      double_lit = token->GetFloatLit();
      ident = token->GetIdentifier();
      token_type = token->GetType();
      file_name = token->GetFileName();
    }
	
	  inline const wstring GetFileName() {
      return file_name;
    }

    inline void SetFileName(wstring f) {
      file_name = f;
    }

    inline const unsigned int GetLineNumber() {
      return line_num;
    }

	  inline void SetLineNbr(unsigned int l) {
      line_num = l;
    }
  
    inline void  SetIntLit(INT_T i) {
      int_lit = i;
    }

    inline void SetFloatLit(FLOAT_T d) {
      double_lit = d;
    }

    inline void SetByteLit(BYTE_T b) {
      byte_lit = b;
    }

    inline void SetCharLit(CHAR_T c) {
      char_lit = c;
    }

    inline void SetIdentifier(wstring i) {
      ident = i;
    }

    inline const INT_T GetIntLit() {
      return int_lit;
    }

    inline const FLOAT_T GetFloatLit() {
      return double_lit;
    }

    inline const BYTE_T GetByteLit() {
      return byte_lit;
    }

    inline const CHAR_T GetCharLit() {
      return char_lit;
    }

    inline const wstring GetIdentifier() {
      return ident;
    }

    inline const enum ScannerTokenType GetType() {
      return token_type;
    }

    inline void SetType(enum ScannerTokenType t) {
      token_type = t;
    }
  };

  /**********************************
   * Token scanner with k lookahead
   * tokens
   **********************************/
  class Scanner {
   private:
	  // input file name
    wstring file_name;
	  // line number
    unsigned int line_num;
    // input buffer
    wchar_t* buffer;
    // buffer size
    size_t buffer_size;
    // input buffer position
    size_t buffer_pos;
    // start marker position
    int start_pos;
    // end marker position
    int end_pos;
    // input characters
    wchar_t cur_char, nxt_char, nxt_nxt_char;
    // map of reserved identifiers
    map<const wstring, enum ScannerTokenType> ident_map;
    // array of tokens for lookahead
    Token* tokens[LOOK_AHEAD];
	
    // warning message
    void ProcessWarning() {
      wcout << L"Parse warning: Unknown token: '" << cur_char << L"'" << endl;
    }
	
    // parsers a character wstring
    inline void CheckString(int index) {
      // copy wstring
      const int length = end_pos - start_pos;
      wstring char_string(buffer, start_pos, length);
      // set wstring
      tokens[index]->SetType(TOKEN_CHAR_STRING_LIT);
		  tokens[index]->SetLineNbr(line_num);
      tokens[index]->SetIdentifier(char_string);
		  tokens[index]->SetFileName(file_name);
    }

    // parse an integer
    inline void ParseInteger(int index, int base = 0) {
      // copy wstring
      int length = end_pos - start_pos;
      wstring ident(buffer, start_pos, length);

      // set token
      wchar_t* end;
      tokens[index]->SetType(TOKEN_INT_LIT);
		  tokens[index]->SetLineNbr(line_num);
		  tokens[index]->SetFileName(file_name);
      tokens[index]->SetIntLit(wcstol(ident.c_str(), &end, base));
    }

    // parse a double
    inline void ParseDouble(int index) {
      // copy wstring
      const int length = end_pos - start_pos;
      wstring wident(buffer, start_pos, length);
      // set token
      tokens[index]->SetType(TOKEN_FLOAT_LIT);
		  tokens[index]->SetLineNbr(line_num);
		  tokens[index]->SetFileName(file_name);
      const string ident(wident.begin(), wident.end());
      tokens[index]->SetFloatLit(atof(ident.c_str()));
    }

    // parsers an unicode character
    inline void ParseUnicodeChar(int index) {
      // copy wstring
      const int length = end_pos - start_pos;
      wstring ident(buffer, start_pos, length);
      // set token
      wchar_t* end;
      tokens[index]->SetType(TOKEN_CHAR_LIT);
		  tokens[index]->SetLineNbr(line_num);
		  tokens[index]->SetFileName(file_name);
      tokens[index]->SetCharLit((wchar_t)wcstol(ident.c_str(), &end, 16));
    }


    // reads a file into memory
	  void ReadFile(const wstring &name);
    // reads a line as input
    void ReadLine(const wstring &line);
    // ignore white space
    void Whitespace();
    // next character
    void NextChar();
    // load reserved keywords
    void LoadKeywords();
    // parses a new token
    void ParseToken(int index);
    // check identifier
    void CheckIdentifier(int index);

   public:
    // default constructor
    Scanner(const wstring &name, bool is_file = true);
    // default destructor
    ~Scanner();

    // next token
    void NextToken();

    // token accessor
    Token* GetToken(int index = 0);
  };
}

#endif
