# TODO: Want everything that's the same between the two to be colored green, and the discrepancies in red.
# Think implementing this feature will benefit from the techniques in the "Crafting Interpreters" book.
# 1. Establish that all 

# detect_discrepancies :: (String, String) -> (String, String)
# compare_ast :: (AST, AST) -> (AST, AST)

# detect_discrepancies invokes compare_ast . parse_ast
# When discrepancies are found within the compare_ast function, the portion of the two asts which caused the discrepancy will be updated
# with "discrepancy in the data structure".colorize(:red), otherwise it'll be colored green if no issue was detgected.

# Lexing process:
# The start of a dictionary: {"#sapphire.off" => 
# The start of a String: "arbitrary series of chars
# The start of a tuple: { Any
# The start of an Array: [ Any
# Other data structures I may be missing?
# The start of a class or a struct: UnquotedTextANdCapitalizedText( @field=100, (consider that at the start of each, a new context
# is entered which dictates the finite set of valid terminating data types/syntax which are acceptable for creating a termination, where termination
# is combined with an initial in order to combine the whole).
# 

# ^^ The functions responsible for this process should be broken up, such that you may pass in the stack of the initials AND the
# array of terminations, and the remaining string in order to push a new initial on to the stack OR pop off an initial from the stack and combine it
# with an initial.
# The controlling function which invokes the function (for the argument (String, String) which is passed to compare_ast)
# described in the previous paragraph will maintain a count of the number of elements in the
# returned terminating list, if the count is incremented for one and not the other.... then the rest of the strings will not match and
# the remaining spec 'got' String output should be colored red.
# Otherwise, if the counts are incremented simultaneously, then do a comparison of the data structures inside the termination and color
# them green or red, and continue the process.

# Helpful notes:
# https://crystal-lang.org/api/0.33.0/String.html#split(separator:Regex,limit=nil,*,remove_empty=false,&block:String-%3E_)-instance-method
# "one".split(\\) => ["o", "n", "e"]

# Solid discussion on the Result Monad and Error Handling
# https://forum.crystal-lang.org/t/result-monad-discussion/1846/16?u=jamesgood626

module SpecResultLexer
# extend self

def string_to_characters(s : String) : Array(String)
    s.split(//)
end

# TODO:
# Need to make the distinction between Compound and Primitive types.
# If a Primitive type is terminated and the next element to be popped off of the @starting_lexed array is a Compound type,
# Then append the Primitive type which was just terminated to the last starting_lexed Compound type,
# Otherwise, push the Primitive type to the @terminated_lexed array
# ^^ A concern to be handled in the lex method.

def determine_mode(first_char : String) : LexMode?
    # { could be a tuple... wouldn't know it's a dict until a => is reached.
    string_starting_chars = ["\""]
    int_starting_chars = /^[0-9]+$/
    float_starting_chars = /^[0-9.]+$/ # TODO: check that the . at the end of the 0-9 will work for matching on floats
    # dict_starting_chars = ["{", "=", ">"]
    tuple_starting_chars = ["{"]
    array_starting_chars = ["["]
    proc_literal_starting_chars = ["#"]

    m = {
        "string" => string_starting_chars,
        "int" => int_starting_chars,
        "float" => float_starting_chars,
        # "dict" => dict_starting_chars,
        "tuple" => tuple_starting_chars,
        "array" => array_starting_chars,
        "proc_literal" => proc_literal_starting_chars,
    }

    result = {} of String => Bool

    m.each do |key, value|
        if value.is_a?(Array)
      		result[key] = value.map { |x| x == first_char }.select { |x| x }.size > 0
        else
            result[key] = (value.match(first_char).is_a?(String))
        end
    end

    mode = nil

    result.each do |key, value|
        if value
            mode =
                case key
                when "string"
                    StringM.new()
                when "int"
                    IntM.new()
                when "float"
                    FloatM.new()
                when "tuple"
                    TupleM.new()
                when "array"
                    ArrayM.new()
                when "proc_literal"
                    ProcLiteralM.new()
                # TODO: when "object"
                end
        end
    end

    return mode

    # Test output from Crystal Playground:
    # {"string" => false,
    # "int" => false,
    # "float" => false,
    # "tuple" => true,
    # "array" => false}

    # NOTES:
    # {"sdadasd", # <- Should always be Mode=TupleM (a Compound type)
    # {"sdadasd" => # <- Should switch from Mode=TupleM to Mode=DictM

    # int_starting_chars.match("1000") : String | Nil

    # In the case of unexpected matches, i.e. "\\n" for newlines or " " for spaces
    # Nil will be returned, in which case it should just be automatically appended to the last StartingToken. 
end

def invoke_lex_on_expected_and_got(expected : Lex, got : Lex) : Bool?
    # If this is true then the lex will stop and the remainder of the got string will be colored red.
    if (@expected.remaining.size == 0 && @got.remaining.size == 0)
        true
    else
        expected.lex()
        got.lex()
    end
end

def starting_or_ending_token_discrepancy(l1 : Lex, l2 : Lex)
    (l1.appended_starting_token != l2.appended_starting_token) || (l2.appended_ending_token != l2.appended_ending_token)
end

def starting_token_append_discrepancy(l1 : Lex, l2 : Lex)
    (l1.appended_to_starting_token != l2.appended_to_starting_token)
end

def determine_if_terminated(l : Lex, type_level : TypeLevel, first_char_of_str : String, terminating_chars : Array(String), func)
    result = terminating_chars.map { |x| x == first_char_of_str }.select { |x| x.is_a?(String) }

    if result.size > 0
        l.ending_tokens.push(TokenEnd.new({type_level, first_char_of_str}))
        l.set_appended_ending_token()
    else
        func.call()
    end
end

def terminate_lex(s1 : String, s2 : String)
    "#{s1}#{s2}"
end

# TODO/LLO
# Other idea instead of the lex_type to establish the distinction....
# pass in an optional lambda function, which will handle the colorization
# ^^ Like this idea/API better
def abort_detect_process(l : Lex)
    case l.lex_type
    when Expected
        # 1. append all of the strings contained in the Array(TokenEnd)
        # 2. append all of the strings contained in the Array(TokenStart)
        # 3. x = "#{appended_token_start_str}#{appended_token_end_str}"
        # 4. "#{x}#{remaining}" 
    when Got
        # 1. append all of the strings contained in the Array(TokenEnd)
        # 2. append all of the strings contained in the Array(TokenStart)
        # 3. x = "#{appended_token_start_str}#{appended_token_end_str}"
        # 4. "#{x}#{remaining}" <- .colorize(:red) if it's the @got lex...
    end
end

# abstract struct StringM end
# abstract struct IntM end
# abstract struct FloatM end
# abstract struct TupleM end
# abstract struct ArrayM end
# abstract struct DictM end
# abstract struct ObjectM end
# alias LexMode = StringM | IntM | FloatM | TupleM | ArrayM | DictM | ObjectM

abstract struct LexMode end
struct StringM < LexMode end
struct IntM < LexMode end
struct FloatM < LexMode end
struct TupleM < LexMode end
struct ArrayM < LexMode end
struct DictM < LexMode end
struct ObjectM < LexMode end
# TODO (may need to add another set of checks of when structs are printed the way TokeenStart is here...
# In the sapphire project, when rtunning the specs for generate_state_map.cr the structs aren't printed starting with '#<'):
# #<SpecResultLexer::TokenStart:0x10c3f5c30 @value={SpecResultLexer::DictM(), "{\"dict_key\"="}>
# #<Proc(String, Nil):0x109fac5d0> <- as printed in the spec results
struct ProcLiteralM < LexMode end

# abstract struct PrimitiveType end
# abstract struct CompoundType end
# alias TypeLevel = PrimitiveType | CompoundType

# Constraint:
# If a PrimitiveType is terminatedAND the last element of the
# starting_tokens array is a CompoundType, then the string held in the PrimitiveType
# will be appended to the string held in the CompoundType.
# ^^ Perhaps this constraint can better be maintained by including a wrapper type around
# @starting_tokens : Array(TokenStart) that indicates when the last element of the array is
# a CompoundType, then this choice becomes a simple case when statement.
abstract struct TypeLevel end
struct PrimitiveType < TypeLevel end
struct CompoundType < TypeLevel end

class TokenStart
    @value : Tuple(LexMode, String)

    getter value

    def initialize(@value) end

    def append(x : String)
        @value = {@value[0], "#{@value[1]}#{x}"}
    end

    def update_mode(m : LexMode)
        @value = {m, @value[1]}
    end
end

class TokenEnd
    @value : Tuple(TypeLevel, String)

    getter value

    def initialize(@value) end
end

# DOESN'T WORK:
# abstract struct Expected end
# abstract struct Got end
# alias LexType = Expected | Got

abstract struct LexType end

struct Expected < LexType end
struct Got < LexType end

class Lex
    @lex_type : LexType
    @remaining : Array(String)

    @starting_tokens : Array(TokenStart)
    @ending_tokens : Array(TokenEnd)

    @appended_starting_token : Bool
    @appended_ending_token : Bool
    @appended_to_starting_token : Bool

    getter remaining
    getter starting_tokens
    getter ending_tokens
    getter appended_starting_token
    getter appended_ending_token
    getter appended_to_starting_token

    def initialize(
        @lex_type,
        @remaining,
        @starting_tokens = [] of TokenStart,
        @ending_tokens = [] of TokenEnd,
        @appended_starting_token = false,
        @appended_ending_token = false,
        @appended_to_starting_token = false
        )
    end

    def lex()
        # NOTE: The invoke_lex_on_got_and_expected ensures that this function won't be invoked under this invariant case.
        raise "OS DEV ERROR: Lex's lex method may not be invoked when @remaining.size == 0." if @remaining.size == 0

        first_char_of_str = @remaining.shift()

        if @starting_tokens.size == 0
            mode = determine_mode(first_char_of_str)
            
            if mode == nil
                # In the case that some character which isn't a syntax character for a given context (StringM, DictM, etc) is encountered.
                self.append_to_last_starting_token(first_char_of_str)
                @appended_to_starting_token = true
            else
                # LLO/WTF... THis annoying error again,
                # In src/spec_result_lexer/impl.cr:221:20

                # 221 | def initialize(@value) end
                #       ^-----
                # Error: instance variable '@value' of SpecResultLexer::TokenStart must be Tuple(SpecResultLexer::LexMode, String), not Tuple(SpecResultLexer::LexMode | Nil, String)

                # Using TupleM.new() in place of mode fixes the error and makes the tests pass...
                @starting_tokens.push(TokenStart.new({mode, first_char_of_str}))
                @appended_starting_token = true
            end

            return
        end

        # puts typeof(@starting_tokens)
        # INVARIANTS:
        # - mode should NEVER be nil
        mode, last_starting_token = @starting_tokens[-1].value

        # There's another comment above about this.
        # When the Mode=TupleM, it's reasonable to assume that if a '=' is encountered that it's safe to set mode to DictM

        # Just sketching this idea out, will need to refactor
        # Idea for how to clean this up...
        # make a multiple dispatch function which takes (mode, ...)
        # Make CONSTANTS at the top for the terminating_chars arrays
        # and do the damn thing in free functions.

        # Damn... still need to take into consideration lambda functions as a possible value
        case mode
        when StringM
            string_terminating_chars = ["\""] # edge case: What if it's an escaped double quotation within a larger String?

            result = string_terminating_chars.map { |x| x == first_char_of_str }.select { |x| x.is_a?(String) }
            
            if result.size > 0
                # IMMEDIATE TODO: Where tf is ending_token coming from?
                # ^^ Test the lex method!
                @ending_tokens.push(TokenEnd.new({PrimitiveType.new(), first_char_of_str}))
                @appended_ending_token = true
            else
                self.append_to_last_starting_token(first_char_of_str)
                @appended_to_starting_token = true
            end
        when IntM
            int_terminating_chars = [" ", "\n", ","]

            # Not sure if the reference to self will be maintained in the scope of the proc literal...
            # might via closure though.
            # ^^ After a quick test... it does work.
            f = ->() {
                self.append_to_last_starting_token(first_char_of_str)
                @appended_to_starting_token = true
            }

            # An attempt to reduce duplication
            determine_if_terminated(self, PrimitiveType.new(), first_char_of_str, int_terminating_chars, f)

            # result = int_terminating_chars.map { |x| x.match(first_char_of_str) }.select { |x| x.is_a?(String) }
            
            # if result.size > 0
            #     @ending_tokens.push(TokenEnd.new({Primitive.new(), ending_token}))
            #     @appended_ending_token = true
            # else
            #     self.append_to_last_starting_token(first_char_of_str)
            #     @appended_to_starting_token = true
            # end
        when FloatM
            float_terminating_chars = [" ", "\n", ","]
        when ArrayM
            array_terminating_chars = ["]"]

            # @ending_tokens.push(TokenEnd.new({Compound, ending_token}))
            # @appended_ending_token = true
        when TupleM
            tuple_terminating_chars = ["}"]

            f = ->() {
                if first_char_of_str == "="
                    self.update_last_starting_token_mode(DictM.new())
                    self.append_to_last_starting_token(first_char_of_str)
                else
                    self.append_to_last_starting_token(first_char_of_str)
                end

                @appended_to_starting_token = true
            }

            # An attempt to reduce duplication
            determine_if_terminated(self, PrimitiveType.new(), first_char_of_str, tuple_terminating_chars, f)

            # result = tuple_terminating_chars.map { |x| x.match(first_char_of_str) }.select { |x| x.is_a?(String) }
            
            # if result.size > 0
            #     @ending_tokens.push(TokenEnd.new({Compound.new(), ending_token}))
            #     @appended_ending_token = true
            # else
            #     if first_char_of_str == "="
            #         self.update_last_starting_token_mode(DictM.new())
            #         self.append_to_last_starting_token(first_char_of_str)
            #     else
            #         self.append_to_last_starting_token(first_char_of_str)
            #     end

            #     @appended_to_starting_token = true
            # end
        when DictM
            dict_terminating_chars = ["}"] # [" ", "\n", ","] <- These are actually the case which should just be appended to the end of a TokenStart

            result = dict_terminating_chars.map { |x| x == first_char_of_str }.select { |x| x.is_a?(String) }
            
            if result.size > 0
                @ending_tokens.push(TokenEnd.new({CompoundType.new(), first_char_of_str}))
                @appended_ending_token = true
            else
                self.append_to_last_starting_token(first_char_of_str)
                @appended_to_starting_token = true
            end

        when ObjectM
            object_terminating_chars = [")"]
        when ProcLiteralM
            proc_literal_terminating_chars = [">"]
        else
            @appended_to_starting_token = true
        end
    end

    def update_last_starting_token_mode(m : LexMode)
        last_token = @starting_tokens.pop()
        last_token.update_mode(m)
        @starting_tokens.push(last_token)
    end

    def append_to_last_starting_token(x : String)
        last_token = @starting_tokens.pop()
        last_token.append(x)
        @starting_tokens.push(last_token)
    end

    def set_appended_ending_token()
        @appended_ending_token = true
    end

    def reset_appended_starting_token_bool()
        @appended_starting_token = false
    end

    def reset_appended_ending_token_bool()
        @appended_ending_token = false
    end
end

class DetectExpectationDiscrepancies
    # extend self
    @expected : Lex
    @got : Lex

    getter expected
    getter got

    def initialize(expected : String, got : String)
        @expected = Lex.new(Expected.new(), string_to_characters(expected))
        @got = Lex.new(Got.new(), string_to_characters(got))
    end

    # "the controlling function" as referred to in the comments above.
    def detect : Tuple(String, String)
        remaining_do_not_match = false

        loop do
            remaining_do_not_match = invoke_lex_on_expected_and_got(@expected, @got)
            break if remaining_do_not_match

            remaining_do_not_match = starting_or_ending_token_discrepancy(@expected, @got) || starting_token_append_discrepancy(@expected, @got)
            break if remaining_do_not_match

            # 2. If a new ending token was added to both, then compare the two, and color the @got one red if the boolean check returns false
            # IMMEDIATE TODO (after eliminating some duplication)

            # ... any other steps between?

            # 3. reset the booleans
        end

        if remaining_do_not_match
            expected = abort_detect_process(@expected)
            got = abort_detect_process(@got)

            return {expected, got}
        else

        end
    end

end

# Sidebar:
# Interesting syntax here (from /spec/spec_helper.cr)
# (1)
# private def apply_program_flags(target)
#   ENV["CRYSTAL_SPEC_COMPILER_FLAGS"]?.try { |f| target.concat(f.split) }
# end
# (2) delegate in a Class declaration
# class Crystal::SpecRunOutput
#     @output : String
  
#     def initialize(@output)
#     end
  
#     def to_string
#       @output
#     end
  
#     delegate to_i, to_i64, to_u64, to_f, to_f32, to_f64, to: @output

#     ...
# end
end