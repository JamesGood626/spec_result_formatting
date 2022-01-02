require "../spec_helper"
require "../../src/**"

include SpecResultLexer

# /Users/jamesgood/desktop/crystal/bin/crystal spec "./spec/spec_result_formatting/detect_expectation_discrepancies_spec.cr"
# crystal spec "./spec/spec_result_lexer/impl_spec.cr"

# describe "context" do
#     it "is able to lex dictionaries" do
#       x = "{
#           \"name\" => \"james\",
#           \"age\" => 29,
#       }"

#       y = Lex.new(Expected.new(), string_to_characters(x))
#       y.should eq("two")
#     end
# end

describe "auxiliary functions" do
    it "determine_mode/1 detects start of a tuple" do
      x = "{\"james\", 29}"

      first_char = string_to_characters(x)[0]
      y = determine_mode(first_char)
      y.should eq(SpecResultLexer::TupleM.new())
    end

    # NOTE:
    # Somewhat ambiguous error message regarding failure to index into a tuple (I needed
    # to unwrap the tuple value from a TokenStart before doing the tuple index):
    # In src/spec_result_lexer/impl.cr:297:17
    # 297 | mode = t[0]
    # Error: undefined method '[]' for SpecResultLexer::TokenStart
    it "The Lex class method lex/0 detects start of a dictionary after being mode TupleM." do
      l = Lex.new(
        Expected.new(),
        ["=", ">"],
        [TokenStart.new({TupleM.new(), "{\"dict_key\""})],
      )

      l.lex()

      mode, starting_tokens = l.starting_tokens[0].value
      expected_mode = DictM.new()
      expected_starting_tokens = "{\"dict_key\"="
      mode.should eq(expected_mode)
      starting_tokens.should eq(expected_starting_tokens)
    end
end

# describe "DetectExpectationDiscrepancies class" do
#   it "can detect and color discrepancies when given two dicts" do
#     expected = "{\"name\" => \"James\"}"
#     got = "{\"name\" => \"Joseph\"}"

#     l = DetectExpectationDiscrepancies.new(expected, got)

#     l.should eq("BOOM")
#   end
# end

# Might be an issue...
# l.starting_tokens.should eq("BOOM") where l : Lex
# [#<SpecResultLexer::TokenStart:0x109d31c30 @value={SpecResultLexer::DictM(), "{\"dict_key\"="}>]

# ^^ Why is a struct being printed with a starting token of #?

# Hmm... even running into issues with the memory address not allowing for equality checks:
# Failure/Error: l.starting_tokens.should eq(expected)

# Expected: [#<SpecResultLexer::TokenStart:0x10c362c00 @value={SpecResultLexer::DictM(), "{\"dict_key\"="}>]
#     got: [#<SpecResultLexer::TokenStart:0x10c362c30 @value={SpecResultLexer::DictM(), "{\"dict_key\"="}>]