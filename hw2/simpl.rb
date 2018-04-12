module TokenKind
    Program = 0
    Statements = 1
    Statement = 2
    Semicolon = 3
    Identifier = 4 # variable
    Assign = 5
    AddOp = 6
    If = 7
    LExpr = 8 # logical expression
    Then = 9
    Else = 10
    End = 11
    For = 12
    From = 13
    To = 14
    Do = 15
    By = 16
    MulOp = 17
    Add = 18
    Subtract = 19
    Factor = 20
    Multiply = 21
    Divide = 22
    Integer = 23
    OpenParen = 24
    CloseParen = 25
    LTerm = 26
    Not = 27
    LFactor = 28
    True = 29
    False = 30
    RelOp = 31
    LessThanOrEqualTo = 32
    LessThan = 33
    Equal = 34
    Comment = 35
    EOF = 36
    And = 37
end

class Token

    def initialize(type, data)
        @token_type = type
        @token_data = data
    end

    def getTokenKind()
        return @token_type
    end

    def getTokenText()
        return @token_data
    end

    def addChild(token)
        @token_data.push(token)
    end

    def getTokenKindString()
        case @token_type
        when 0
            return "Program"
        when 1
            return "Statements"
        when 2
            return "Statement"
        when 3
            return "Semicolon"
        when 4
            return "Identifier"
        when 5
            return "Assign"
        when 6
            return "AddOp"
        when 7
            return "If"
        when 8
            return "LExpr"
        when 9
            return "Then"
        when 10
            return "Else"
        when 11
            return "End"
        when 12
            return "For"
        when 13
            return "From"
        when 14
            return "To"
        when 15
            return "Do"
        when 16
            return "By"
        when 17
            return "MulOp"
        when 18
            return "Add"
        when 19
            return "Subtract"
        when 20
            return "Factor"
        when 21
            return "Multiply"
        when 22
            return "Divide"
        when 23
            return "Integer"
        when 24
            return "OpenParen"
        when 25
            return "CloseParen"
        when 26
            return "LTerm"
        when 27
            return "Not"
        when 28
            return "LFactor"
        when 29
            return "True"
        when 30
            return "False"
        when 31
            return "RelOp"
        when 32
            return "LessThanOrEqualTo"
        when 33
            return "LessThan"
        when 34
            return "Equal"
        when 35
            return "Comment"
        when 36
            return "EOF"
        when 37
            return "And"
        end
    end

    def inspect()
        return "#{getTokenKindString()}, \"#{@token_data}\"."
    end

    def to_s()
        return "#{getTokenKindString()}, \"#{@token_data}\"."
    end

end

# Converts input file into list of tokens
class Lexer

    def initialize(file_path)
        @file = File.read(file_path).split('')
        @tokens = Array.new()

        generateTokenList()
    end

    def getTokenKind()
        return @tokens[0].getTokenKind()
    end

    def getTokenText()
        return @tokens[0].getTokenText()
    end

    def nextToken()
        return @tokens.shift()
    end

private

    # Generate a list of tokens, doesn't check for validity
    def generateTokenList()
        cur_pos = 0
        until cur_pos == @file.length do
            case @file[cur_pos]
            when /[a-zA-Z]/
                # Word
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ /[a-zA-Z0-9_]/)
                    string << @file[cur_pos]
                    cur_pos += 1
                end

                case string
                when "if"
                    @tokens.push(Token.new(TokenKind::If, string))
                when "then"
                    @tokens.push(Token.new(TokenKind::Then, string))
                when "end"
                    @tokens.push(Token.new(TokenKind::End, string))
                when "else"
                    @tokens.push(Token.new(TokenKind::Else, string))
                when "for"
                    @tokens.push(Token.new(TokenKind::For, string))
                when "from"
                    @tokens.push(Token.new(TokenKind::From, string))
                when "to"
                    @tokens.push(Token.new(TokenKind::To, string))
                when "do"
                    @tokens.push(Token.new(TokenKind::Do, string))
                when "by"
                    @tokens.push(Token.new(TokenKind::By, string))
                when "and"
                    @tokens.push(Token.new(TokenKind::And, string))
                when "not"
                    @tokens.push(Token.new(TokenKind::Not, string))
                when "true"
                    @tokens.push(Token.new(TokenKind::True, string))
                when "false"
                    @tokens.push(Token.new(TokenKind::False, string))
                else
                    @tokens.push(Token.new(TokenKind::Identifier, string))
                end
            when /[0-9]/
                # Integer
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ /[0-9]/)
                    string << @file[cur_pos]
                    cur_pos += 1
                end
                @tokens.push(Token.new(TokenKind::Integer, string))
            when /[+\-*\/<=;:()]/
                # Symbol(s)
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ /[+\-*\/<=;:()]/)
                    string << @file[cur_pos]
                    cur_pos += 1
                end

                # <=, :=, and // are the only multicharacter tokens, rest should be read in
                # one character at a time
                if string == "<="
                    @tokens.push(Token.new(TokenKind::LessThanOrEqualTo, string))
                elsif string == ":="
                    @tokens.push(Token.new(TokenKind::Assign, string))
                elsif string == "//"
                    while (cur_pos != @file.length && @file[cur_pos] != "\n")
                        string << @file[cur_pos]
                        cur_pos += 1
                    end
                    # @tokens.push(Token.new(TokenKind::Comment, string))
                else
                    string.split('').each do |symbol|
                        case symbol
                        when "+"
                            @tokens.push(Token.new(TokenKind::Add, symbol))
                        when "-"
                            @tokens.push(Token.new(TokenKind::Subtract, symbol))
                        when "*"
                            @tokens.push(Token.new(TokenKind::Multiply, symbol))
                        when "/"
                            @tokens.push(Token.new(TokenKind::Divide, symbol))
                        when "<"
                            @tokens.push(Token.new(TokenKind::LessThan, symbol))
                        when "="
                            @tokens.push(Token.new(TokenKind::Equal, symbol))
                        when "("
                            @tokens.push(Token.new(TokenKind::OpenParen, symbol))
                        when ")"
                            @tokens.push(Token.new(TokenKind::CloseParen, symbol))
                        when ";"
                            @tokens.push(Token.new(TokenKind::Semicolon, symbol))
                        end
                    end
                end
            when /\s/
                cur_pos += 1
            else
                raise "Invalid character #{@file[cur_pos]}!"
            end
        end
        @tokens.push(Token.new(TokenKind::EOF, "END_OF_FILE"))
    end
end

# Checks if lexer output is syntactically valid
class Parser

    def initialize(file_path)
        @lexer = Lexer.new(file_path)
    end

    def parse()
        getProgram()
    end

private

    def RaiseParsingException(expected)
        token = @lexer.nextToken()
        raise "Expected #{expected}, got '#{token.getTokenText()}' (#{token.getTokenKindString()})"
    end

    def getProgram()
        program = Token.new(TokenKind::Program, Array.new())
        program.addChild(getStatements())
        puts "Parse successful!"
        return program
    end

    def getStatements()
        statements = Token.new(TokenKind::Statements, Array.new())

        statements.addChild(getStatement())
        if (@lexer.getTokenKind() != TokenKind::EOF)
            statements.addChild(getTerminalToken("';'", TokenKind::Semicolon))
        end
        if (@lexer.getTokenKind() != TokenKind::EOF &&
            @lexer.getTokenKind() != TokenKind::Else &&
            @lexer.getTokenKind() != TokenKind::End)
            statements.addChild(getStatements())
        end

        return statements
    end

    def getStatement()
        statement = Token.new(TokenKind::Statement, Array.new())

        if (@lexer.getTokenKind() == TokenKind::Identifier)
            statement.addChild(getTerminalToken("Identifier", TokenKind::Identifier))
            statement.addChild(getTerminalToken("':='", TokenKind::Assign))
            statement.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::If)
            statement.addChild(getTerminalToken("'if'", TokenKind::If))
            statement.addChild(getLExpr())
            statement.addChild(getTerminalToken("'then'", TokenKind::Then))
            statement.addChild(getStatements())
            if (@lexer.getTokenKind == TokenKind::End)
                statement.addChild(getTerminalToken("'end'", TokenKind::End))
            elsif (@lexer.getTokenKind() == TokenKind::Else)
                statement.addChild(getTerminalToken("'else'", TokenKind::Else))
                statement.addChild(getStatements())
                statement.addChild(getTerminalToken("'end'", TokenKind::End))
            else
                RaiseParsingException("'end' or 'else'")
            end
        elsif (@lexer.getTokenKind() == TokenKind::For)
            statement.addChild(getTerminalToken("'for'", TokenKind::For))
            statement.addChild(getTerminalToken("Identifier", TokenKind::Identifier))
            statement.addChild(getTerminalToken("'from'", TokenKind::From))
            statement.addChild(getAddOp())
            statement.addChild(getTerminalToken("'to'", TokenKind::To))
            statement.addChild(getAddOp())
            if (@lexer.getTokenKind() == TokenKind::Do)
                statement.addChild(getTerminalToken("'do'", TokenKind::Do))
                statement.addChild(getStatements())
                statement.addChild(getTerminalToken("'end'", TokenKind::End))
            elsif (@lexer.getTokenKind() == TokenKind::By)
                statement.addChild(getTerminalToken("'by'", TokenKind::By))
                statement.addChild(getAddOp())
                statement.addChild(getTerminalToken("'do'", TokenKind::Do))
                statement.addChild(getStatements())
                statement.addChild(getTerminalToken("'end'", TokenKind::End))
            else
                RaiseParsingException("'do' or 'by'")
            end
        else
            RaiseParsingException("Identifier, 'if', or 'for'")
        end

        return statement
    end

    def getAddOp()
        addOp = Token.new(TokenKind::AddOp, Array.new())

        addOp.addChild(getMulOp())
        if (@lexer.getTokenKind() == TokenKind::Add)
            addOp.addChild(getTerminalToken("'+'", TokenKind::Add))
            addOp.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::Subtract)
            addOp.addChild(getTerminalToken("'-'", TokenKind::Subtract))
            addOp.addChild(getAddOp())
        end

        return addOp
    end

    def getMulOp()
        mulOp = Token.new(TokenKind::MulOp, Array.new())

        mulOp.addChild(getFactor())
        if (@lexer.getTokenKind() == TokenKind::Multiply)
            mulOp.addChild(getTerminalToken("'*'", TokenKind::Multiply))
            mulOp.addChild(getMulOp())
        elsif (@lexer.getTokenKind() == TokenKind::Divide)
            mulOp.addChild(getTerminalToken("'/'", TokenKind::Divide))
            mulOp.addChild(getMulOp())
        end

        return mulOp
    end

    def getFactor()
        factor = Token.new(TokenKind::Factor, Array.new())

        if (@lexer.getTokenKind() == TokenKind::Integer)
            factor.addChild(getTerminalToken("Integer", TokenKind::Integer))
        elsif (@lexer.getTokenKind() == TokenKind::Identifier)
            factor.addChild(getTerminalToken("Identifier", TokenKind::Identifier))
        elsif (@lexer.getTokenKind() == TokenKind::OpenParen)
            factor.addChild(getTerminalToken("'('", TokenKind::OpenParen))
            factor.addChild(getAddOp())
            factor.addChild(getTerminalToken("')'", TokenKind::CloseParen))
        else
            RaiseParsingException("Integer, Identifer, or '('")
        end

        return factor
    end

    def getLExpr()
        lExpr = Token.new(TokenKind::LExpr, Array.new())

        lExpr.addChild(getLTerm())
        if (@lexer.getTokenKind() == TokenKind::And)
            lExpr.addChild(getTerminalToken("'and'", TokenKind::And))
            lExpr.addChild(getLExpr())
        end

        return lExpr
    end

    def getLTerm()
        lTerm = Token.new(TokenKind::LTerm, Array.new())

        if (@lexer.getTokenKind() == TokenKind::Not)
            lTerm.addChild(getTerminalToken("'not'", TokenKind::Not))
        end
        lTerm.addChild(getLFactor())

        return lTerm
    end

    def getLFactor()
        lFactor = Token.new(TokenKind::LFactor, Array.new())

        if (@lexer.getTokenKind() == TokenKind::True)
            lFactor.addChild(getTerminalToken("'true'", TokenKind::True))
        elsif (@lexer.getTokenKind() == TokenKind::False)
            lFactor.addChild(getTerminalToken("'false'", TokenKind::False))
        end
        lFactor.addChild(getRelOp())

        return lFactor
    end

    def getRelOp()
        relOp = Token.new(TokenKind::RelOp, Array.new())

        relOp.addChild(getAddOp())
        if (@lexer.getTokenKind() == TokenKind::LessThanOrEqualTo)
            relOp.addChild(getTerminalToken("'<='", TokenKind::LessThanOrEqualTo))
            relOp.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::LessThan)
            relOp.addChild(getTerminalToken("'<'", TokenKind::LessThan))
            relOp.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::Equal)
            relOp.addChild(getTerminalToken("'='", TokenKind::Equal))
            relOp.addChild(getAddOp())
        else
            RaiseParsingException("'<=', '<', or '='")
        end
    end

    def getTerminalToken(name, kind)
        if (@lexer.getTokenKind() == kind)
            return @lexer.nextToken()
        else
            RaiseParsingException(name)
        end
    end
end

p = Parser.new("valid.simpl").parse()
puts p
