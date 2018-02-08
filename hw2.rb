module TokenSymbol
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
    EOF = 35
    Comment = 36
end

class Token

    def initialize(type, string)
        @token_type = type
        @token_string = string
    end

    def getTokenKind()
        return @token_type
    end

    def getTokenText()
        return @token_string
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
            return "LExp"
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
            return "EOF"
        when 36
            return "Comment"
        end
    end

    def to_s()
        return "#{getTokenKindString()}, \"#{@token_string}\"."
    end

end


# Converts input file into list of tokens
class Lexer

    def initialize(file_path)
        @file = File.read(file_path).split('')
        @tokens = Array.new()

        generateTokenList()
        puts @tokens
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

    # All this does is generate a list of tokens, doesn't check for validity
    def generateTokenList()
        @tokens.push(Token.new(TokenSymbol::Program, "PROGRAM_START"))
        @tokens.push(Token.new(TokenSymbol::Statements, "STATEMENTS_START"))

        cur_pos = 0
        until cur_pos == @file.length do
            case @file[cur_pos]
            when /[a-zA-Z]/
                # Word
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ /[a-zA-Z]/)
                    string << @file[cur_pos]
                    cur_pos += 1
                end

                case string
                when "if"
                    @tokens.push(Token.new(TokenSymbol::If, string))
                when "then"
                    @tokens.push(Token.new(TokenSymbol::Then, string))
                when "end"
                    @tokens.push(Token.new(TokenSymbol::End, string))
                when "else"
                    @tokens.push(Token.new(TokenSymbol::Else, string))
                when "for"
                    @tokens.push(Token.new(TokenSymbol::For, string))
                when "from"
                    @tokens.push(Token.new(TokenSymbol::From, string))
                when "to"
                    @tokens.push(Token.new(TokenSymbol::To, string))
                when "do"
                    @tokens.push(Token.new(TokenSymbol::Do, string))
                when "by"
                    @tokens.push(Token.new(TokenSymbol::By, string))
                when "and"
                    @tokens.push(Token.new(TokenSymbol::And, string))
                when "not"
                    @tokens.push(Token.new(TokenSymbol::Not, string))
                when "true"
                    @tokens.push(Token.new(TokenSymbol::True, string))
                when "false"
                    @tokens.push(Token.new(TokenSymbol::False, string))
                else
                    @tokens.push(Token.new(TokenSymbol::Identifier, string))
                end
            when /[0-9]/
                # Integer
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ /[0-9]/)
                    string << @file[cur_pos]
                    cur_pos += 1
                end
                @tokens.push(Token.new(TokenSymbol::Integer, string))
            when %r{[+\-*/<=;():]}
                string = ""
                while (cur_pos != @file.length && @file[cur_pos] =~ %r{[+\-*/<=;():]})
                    string << @file[cur_pos]
                    cur_pos += 1
                end

                case string
                when "+"
                    @tokens.push(Token.new(TokenSymbol::Plus, string))
                when "-"
                    @tokens.push(Token.new(TokenSymbol::Minus, string))
                when "*"
                    @tokens.push(Token.new(TokenSymbol::Multiply, string))
                when "/"
                    @tokens.push(Token.new(TokenSymbol::Divide, string))
                when "<"
                    @tokens.push(Token.new(TokenSymbol::LessThan, string))
                when "<="
                    @tokens.push(Token.new(TokenSymbol::LessThanOrEqualTo, string))
                when "="
                    @tokens.push(Token.new(TokenSymbol::Equal, string))
                when "("
                    @tokens.push(Token.new(TokenSymbol::OpenParen, string))
                when ")"
                    @tokens.push(Token.new(TokenSymbol::CloseParen, string))
                when ";"
                    @tokens.push(Token.new(TokenSymbol::Semicolon, string))
                when "//"
                    while (cur_pos != @file.length && @file[cur_pos] != "\n")
                        string << @file[cur_pos]
                        cur_pos += 1
                    end
                    @tokens.push(Token.new(TokenSymbol::Comment, string))
                when ":="
                    @tokens.push(Token.new(TokenSymbol::Assign, string))
                end
            else
                cur_pos += 1
            end
        end
        @tokens.push(Token.new(TokenSymbol::EOF, "END_OF_FILE"))
    end

end

p = Lexer.new("valid.simpl")
