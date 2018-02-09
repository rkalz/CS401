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
    Plus = 18
    Minus = 19
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
            return "Plus"
        when 19
            return "Minus"
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
        end
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

    def isEmpty()
        return @tokens.length == 0
    end

private

    # All this does is generate a list of tokens, doesn't check for validity
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
            when %r{[+\-*/<=;():]}
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ %r{[+\-*/<=;():]})
                    string << @file[cur_pos]
                    cur_pos += 1
                end

                case string
                when "+"
                    @tokens.push(Token.new(TokenKind::Plus, string))
                when "-"
                    @tokens.push(Token.new(TokenKind::Minus, string))
                when "*"
                    @tokens.push(Token.new(TokenKind::Multiply, string))
                when "/"
                    @tokens.push(Token.new(TokenKind::Divide, string))
                when "<"
                    @tokens.push(Token.new(TokenKind::LessThan, string))
                when "<="
                    @tokens.push(Token.new(TokenKind::LessThanOrEqualTo, string))
                when "="
                    @tokens.push(Token.new(TokenKind::Equal, string))
                when "("
                    @tokens.push(Token.new(TokenKind::OpenParen, string))
                when ")"
                    @tokens.push(Token.new(TokenKind::CloseParen, string))
                when ";"
                    @tokens.push(Token.new(TokenKind::Semicolon, string))
                when "//"
                    while (cur_pos != @file.length && @file[cur_pos] != "\n")
                        string << @file[cur_pos]
                        cur_pos += 1
                    end
                    @tokens.push(Token.new(TokenKind::Comment, string))
                when ":="
                    @tokens.push(Token.new(TokenKind::Assign, string))
                end
            else
                cur_pos += 1
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
        raise "Expected #{expected}, got #{@lexer.nextToken().getTokenKindString()}"
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
            statements.addChild(getTerminalToken("semicolon", TokenKind::Semicolon))
            if (@lexer.getTokenKind() != TokenKind::End)
                statements.addChild(getStatements())
            end
        end
        return statements
    end

    def getStatement()
        statement = Token.new(TokenKind::Statement, Array.new())

        if (@lexer.getTokenKind() == TokenKind::Identifier)
            statement.addChild(getTerminalToken("Identifier", TokenKind::Identifier))
            statement.addChild(getTerminalToken(":=", TokenKind::Assign))
            statement.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::If)
            statement.addChild(getTerminalToken("If", TokenKind::If))
            statement.addChild(getLExpr())
            statement.addChild(getTerminalToken("Then", TokenKind::Then))
            statement.addChild(getStatements())
            statement.addChild(getTerminalToken("Else", TokenKind::Else))
            if (@lexer.getTokenKind == TokenKind::End)
                statement.addChild(getTerminalToken("End", TokenKind::End))
            elsif (@lexer.getTokenKind() == TokenKind::Else)
                statement.addChild(getTerminalToken("Else", TokenKind::Else))
                statement.addChild(getStatements())
                statement.addChild(getTerminalToken("End", TokenKind::End))
            else
                RaiseParsingException("End or Else")
            end
        elsif (@lexer.getTokenKind() == TokenKind::For)
            statement.addChild(getTerminalToken("For", TokenKind::For))
            statement.addChild(getTerminalToken("Identifier", TokenKind::Identifier))
            statement.addChild(getTerminalToken("From", TokenKind::From))
            statement.addChild(getAddOp())
            statement.addChild(getTerminalToken("To", TokenKind::To))
            statement.addChild(getAddOp())
            if (@lexer.getTokenKind() == TokenKind::Do)
                statement.addChild(getTerminalToken("Do", TokenKind::Do))
                statement.addChild(getStatements())
                statement.addChild(getTerminalToken("End", TokenKind::End))
            elsif (@lexer.getTokenKind() == TokenKind::By)
                statement.addChild(getTerminalToken("By", TokenKind::By))
                statement.addChild(getAddOp())
                statement.addChild(getTerminalToken("Do", TokenKind::Do))
                statement.addChild(getStatements())
                statement.addChild(getTerminalToken("End", TokenKind::End))
            else
                RaiseParsingException("Do or By")
            end
        else
            RaiseParsingException("Identifier, If, or For")
        end

        return statement
    end

    def getAddOp()
        addOp = Token.new(TokenKind::AddOp, Array.new())

        addOp.addChild(getMulOp())
        if (@lexer.getTokenKind() == TokenKind::Plus)
            addOp.addChild(getTerminalToken("Plus", TokenKind::Plus))
            addOp.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::Minus)
            addOp.addChild(getTerminalToken("Minus", TokenKind::Minus))
            addOp.addChild(getAddOp())
        end

        return addOp
    end

    def getMulOp()
        mulOp = Token.new(TokenKind::MulOp, Array.new())

        mulOp.addChild(getFactor())
        if (@lexer.getTokenKind() == TokenKind::Multiply)
            mulOp.addChild(getTerminalToken("Multply", TokenKind::Multiply))
            mulOp.addChild(getMulOp())
        elsif (@lexer.getTokenKind() == TokenKind::Divide)
            mulOp.addChild(getTerminalToken("Divide", TokenKind::Divide))
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
            factor.addChild(getTerminalToken("(", TokenKind::OpenParen))
            factor.addChild(getAddOp())
            factor.addChild(getTerminalToken(")", TokenKind::CloseParen))
        else
            RaiseParsingException("Integer, Identifer, or (")
        end

        return factor
    end

    def getLExpr()
        lExpr = Token.new(TokenKind::LExpr, Array.new())

        lExpr.addChild(getLTerm())
        if (@lexer.getTokenKind() == TokenKind::And)
            lExpr.addChild(getTerminalToken("And", TokenKind::And))
            lExpr.addChild(getLExpr())
        end

        return lExpr
    end

    def getLTerm()
        lTerm = Token.new(TokenKind::LTerm, Array.new())

        if (@lexer.getTokenKind() == TokenKind::Not)
            lExpr.addChild(getTerminalToken("Not", TokenKind::Not))
        end
        lExpr.addChild(getLFactor())

        return lTerm
    end

    def getLFactor()
        lFactor = Token.new(TokenKind::LFactor, Array.new())

        if (@lexer.getTokenKind() == TokenKind::True)
            lFactor.addChild(getTerminalToken("True", TokenKind::True))
        elsif (@lexer.getTokenKind() == TokenKind::False)
            lFactor.addChild(getTerminalToken("False", TokenKind::False))
        end
        lFactor.addChild(getRelOp())

        return lFactor
    end

    def getRelOp()
        relOp = Token.new(TokenKind::RelOp, Array.new())

        relOp.addChild(getAddOp())
        if (@lexer.getTokenKind() == TokenKind::LessThanOrEqualTo)
            relOp.addChild(getTerminalToken("<=", TokenKind::LessThanOrEqualTo))
            relOp.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::LessThan)
            relOp.addChild(getTerminalToken("<", TokenKind::LessThan))
            relOp.addChild(getAddOp())
        elsif (@lexer.getTokenKind() == TokenKind::Equal)
            relOp.addChild(getTerminalToken("=", TokenKind::LessThan))
            relOp.addChild(getAddOp())
        else
            RaiseParsingException("<=, <, or =")
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

Parser.new("valid.simpl").parse()
puts p
