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

    
end