Nonterminals 
alpha bit char cr lf crlf ctl digit dquote hexdig htab sp wsp anywsp lwsp octet vchar
prose_val prose_inner prose_char
hex_val hex_digits hex_concats hex_concat hex_range
dec_val dec_digits dec_concats dec_concat dec_range
bin_val bin_digits bin_concats bin_concat bin_range
num_val
char_val char_inner char_char
rulename rulename_tail rulename_inner
comment comment_tail comment_char
c_nl c_wsp
defined_as
element elements
repeat digits repetition
concatenation concatenation_tail concatenation_inner
alternation alternation_tail alternation_tail2
group option rule 
rulelist rulelist_tail rulelist_inner.

Terminals 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255.

Rootsymbol rulelist.


prose_val -> 60 prose_inner 62 : {prose, '$2'}.
prose_inner -> prose_char : ['$1'].
prose_inner -> prose_char prose_inner : ['$1' | '$2'].

hex_val -> 120 hex_digits : hexval('$2').
hex_val -> 120 hex_digits hex_concats : {seq, [hexval(X) || X <- ['$2' | '$3']]}.
hex_val -> 120 hex_digits hex_range : {range, hexval('$2'), hexval('$3')}.

hex_digits -> hexdig : ['$1'].
hex_digits -> hexdig hex_digits : ['$1' | '$2'].

hex_concats -> hex_concat : ['$1'].
hex_concats -> hex_concat hex_concats : ['$1' | '$2'].

hex_concat -> 46 hex_digits : '$2'.

hex_range -> 45 hex_digits : '$2'.


dec_val -> 100 dec_digits : decval('$2').
dec_val -> 100 dec_digits dec_concats : {seq, [decval(X) || X <- ['$2' | '$3']]}.
dec_val -> 100 dec_digits dec_range : {range, decval('$2'), decval('$3')}.

dec_digits -> digit : ['$1'].
dec_digits -> digit dec_digits : ['$1' | '$2'].

dec_concats -> dec_concat : ['$1'].
dec_concats -> dec_concat dec_concats : ['$1' | '$2'].

dec_concat -> 46 dec_digits : '$2'.

dec_range -> 45 dec_digits : '$2'.


bin_val -> 98 bin_digits : binval('$2').
bin_val -> 98 bin_digits bin_concats : {seq, [binval(X) || X <- ['$2' | '$3']]}.
bin_val -> 98 bin_digits bin_range : {range, binval('$2'), binval('$3')}.

bin_digits -> bit : ['$1'].
bin_digits -> bit bin_digits : ['$1' | '$2'].

bin_concats -> bin_concat : ['$1'].
bin_concats -> bin_concat bin_concats : ['$1' | '$2'].

bin_concat -> 46 bin_digits : '$2'.

bin_range -> 45 bin_digits : '$2'.


num_val -> 37 hex_val : '$2'.
num_val -> 37 dec_val : '$2'.
num_val -> 37 bin_val : '$2'.


char_val -> 34 char_inner 34 : {char, '$2'}.

char_inner -> char_char : ['$1'].
char_inner -> char_char char_inner : ['$1' | '$2'].


rulename -> alpha rulename_tail : {rulename, ['$1' | '$2']}.

rulename_tail -> rulename_inner : ['$1'].
rulename_tail -> rulename_inner rulename_tail : ['$1' | '$2'].

rulename_inner -> alpha : '$1'.
rulename_inner -> digit : '$1'.
rulename_inner -> 45 : 45.

comment -> 59 comment_tail crlf : {comment, '$2'}.

comment_tail -> comment_char : ['$1'].
comment_tail -> comment_char comment_tail : ['$1' | '$2'].

comment_char -> wsp : '$1'.
comment_char -> vchar : '$1'.

c_nl -> comment : '$1'.
c_nl -> crlf : '$1'.

c_wsp -> wsp : '$1'.
c_wsp -> c_nl wsp : '$1'.

defined_as -> 61 : eq.
defined_as -> 61 47 : alt.


element -> rulename : '$1'.
element -> char_val : '$1'.
element -> num_val : '$1'.
element -> prose_val : '$1'.
element -> group : '$1'.
element -> option : '$1'.

repeat -> digits : {repeat, '$1'}.
repeat -> 42 : {repeat, any}.
repeat -> digits 42 : {repeat, from, '$1'}.
repeat -> 42 digits : {repeat, to, '$2'}.
repeat -> digits 42 digits : {repeat, '$1', '$3'}.

digits -> digit : ['$1'].
digits -> digit digits : ['$1' | '$2'].


repetition -> element : '$1'.
repetition -> repeat element : {'$1', '$2'}.


concatenation -> repetition : '$1'.
concatenation -> repetition concatenation_tail : {concatenation, ['$1' | '$2']}.

concatenation_tail -> concatenation_inner : ['$1'].
concatenation_tail -> concatenation_inner concatenation_tail : ['$1' | '$2'].

concatenation_inner -> wsp repetition : '$2'.


alternation -> concatenation : '$1'.
alternation -> concatenation alternation_tail : {alternation, ['$1'|'$2']}.

alternation_tail -> 47 concatenation : ['$2'].
alternation_tail -> 47 concatenation alternation_tail : ['$2'|'$3'].

group -> 40 alternation 41 : '$2'.
option -> 91 alternation 93 : {option, '$2'}.

elements -> alternation : '$1'.

rule -> rulename defined_as elements : {rule, '$1', '$3'}.

rulelist -> rule : ['$1'].
rulelist -> rule rulelist_tail : {rules, ['$1' | '$2']}.

rulelist_tail -> rulelist_inner : ['$1'].
rulelist_tail -> rulelist_inner rulelist_tail: ['$1' | '$2'].

rulelist_inner -> lf rule : '$2'.


char_char -> 32 : 32.
char_char -> 33 : 33.
char_char -> 35 : 35.
char_char -> 36 : 36.
char_char -> 37 : 37.
char_char -> 38 : 38.
char_char -> 39 : 39.
char_char -> 40 : 40.
char_char -> 41 : 41.
char_char -> 42 : 42.
char_char -> 43 : 43.
char_char -> 44 : 44.
char_char -> 45 : 45.
char_char -> 46 : 46.
char_char -> 47 : 47.
char_char -> 48 : 48.
char_char -> 49 : 49.
char_char -> 50 : 50.
char_char -> 51 : 51.
char_char -> 52 : 52.
char_char -> 53 : 53.
char_char -> 54 : 54.
char_char -> 55 : 55.
char_char -> 56 : 56.
char_char -> 57 : 57.
char_char -> 58 : 58.
char_char -> 59 : 59.
char_char -> 60 : 60.
char_char -> 61 : 61.
char_char -> 62 : 62.
char_char -> 63 : 63.
char_char -> 64 : 64.
char_char -> 65 : 65.
char_char -> 66 : 66.
char_char -> 67 : 67.
char_char -> 68 : 68.
char_char -> 69 : 69.
char_char -> 70 : 70.
char_char -> 71 : 71.
char_char -> 72 : 72.
char_char -> 73 : 73.
char_char -> 74 : 74.
char_char -> 75 : 75.
char_char -> 76 : 76.
char_char -> 77 : 77.
char_char -> 78 : 78.
char_char -> 79 : 79.
char_char -> 80 : 80.
char_char -> 81 : 81.
char_char -> 82 : 82.
char_char -> 83 : 83.
char_char -> 84 : 84.
char_char -> 85 : 85.
char_char -> 86 : 86.
char_char -> 87 : 87.
char_char -> 88 : 88.
char_char -> 89 : 89.
char_char -> 90 : 90.
char_char -> 91 : 91.
char_char -> 92 : 92.
char_char -> 93 : 93.
char_char -> 94 : 94.
char_char -> 95 : 95.
char_char -> 96 : 96.
char_char -> 97 : 97.
char_char -> 98 : 98.
char_char -> 99 : 99.
char_char -> 100 : 100.
char_char -> 101 : 101.
char_char -> 102 : 102.
char_char -> 103 : 103.
char_char -> 104 : 104.
char_char -> 105 : 105.
char_char -> 106 : 106.
char_char -> 107 : 107.
char_char -> 108 : 108.
char_char -> 109 : 109.
char_char -> 110 : 110.
char_char -> 111 : 111.
char_char -> 112 : 112.
char_char -> 113 : 113.
char_char -> 114 : 114.
char_char -> 115 : 115.
char_char -> 116 : 116.
char_char -> 117 : 117.
char_char -> 118 : 118.
char_char -> 119 : 119.
char_char -> 120 : 120.
char_char -> 121 : 121.
char_char -> 122 : 122.
char_char -> 123 : 123.
char_char -> 124 : 124.
char_char -> 125 : 125.
char_char -> 126 : 126.


prose_char -> 32 : 32.
prose_char -> 33 : 33.
prose_char -> 34 : 34.
prose_char -> 35 : 35.
prose_char -> 36 : 36.
prose_char -> 37 : 37.
prose_char -> 38 : 38.
prose_char -> 39 : 39.
prose_char -> 40 : 40.
prose_char -> 41 : 41.
prose_char -> 42 : 42.
prose_char -> 43 : 43.
prose_char -> 44 : 44.
prose_char -> 45 : 45.
prose_char -> 46 : 46.
prose_char -> 47 : 47.
prose_char -> 48 : 48.
prose_char -> 49 : 49.
prose_char -> 50 : 50.
prose_char -> 51 : 51.
prose_char -> 52 : 52.
prose_char -> 53 : 53.
prose_char -> 54 : 54.
prose_char -> 55 : 55.
prose_char -> 56 : 56.
prose_char -> 57 : 57.
prose_char -> 58 : 58.
prose_char -> 59 : 59.
prose_char -> 60 : 60.
prose_char -> 61 : 61.
prose_char -> 63 : 63.
prose_char -> 64 : 64.
prose_char -> 65 : 65.
prose_char -> 66 : 66.
prose_char -> 67 : 67.
prose_char -> 68 : 68.
prose_char -> 69 : 69.
prose_char -> 70 : 70.
prose_char -> 71 : 71.
prose_char -> 72 : 72.
prose_char -> 73 : 73.
prose_char -> 74 : 74.
prose_char -> 75 : 75.
prose_char -> 76 : 76.
prose_char -> 77 : 77.
prose_char -> 78 : 78.
prose_char -> 79 : 79.
prose_char -> 80 : 80.
prose_char -> 81 : 81.
prose_char -> 82 : 82.
prose_char -> 83 : 83.
prose_char -> 84 : 84.
prose_char -> 85 : 85.
prose_char -> 86 : 86.
prose_char -> 87 : 87.
prose_char -> 88 : 88.
prose_char -> 89 : 89.
prose_char -> 90 : 90.
prose_char -> 91 : 91.
prose_char -> 92 : 92.
prose_char -> 93 : 93.
prose_char -> 94 : 94.
prose_char -> 95 : 95.
prose_char -> 96 : 96.
prose_char -> 97 : 97.
prose_char -> 98 : 98.
prose_char -> 99 : 99.
prose_char -> 100 : 100.
prose_char -> 101 : 101.
prose_char -> 102 : 102.
prose_char -> 103 : 103.
prose_char -> 104 : 104.
prose_char -> 105 : 105.
prose_char -> 106 : 106.
prose_char -> 107 : 107.
prose_char -> 108 : 108.
prose_char -> 109 : 109.
prose_char -> 110 : 110.
prose_char -> 111 : 111.
prose_char -> 112 : 112.
prose_char -> 113 : 113.
prose_char -> 114 : 114.
prose_char -> 115 : 115.
prose_char -> 116 : 116.
prose_char -> 117 : 117.
prose_char -> 118 : 118.
prose_char -> 119 : 119.
prose_char -> 120 : 120.
prose_char -> 121 : 121.
prose_char -> 122 : 122.
prose_char -> 123 : 123.
prose_char -> 124 : 124.
prose_char -> 125 : 125.
prose_char -> 126 : 126.

% A - Z
alpha -> 65 : 65.
alpha -> 66 : 66.
alpha -> 67 : 67.
alpha -> 68 : 68.
alpha -> 69 : 69.
alpha -> 70 : 70.
alpha -> 71 : 71.
alpha -> 72 : 72.
alpha -> 73 : 73.
alpha -> 74 : 74.
alpha -> 75 : 75.
alpha -> 76 : 76.
alpha -> 77 : 77.
alpha -> 78 : 78.
alpha -> 79 : 79.
alpha -> 80 : 80.
alpha -> 81 : 81.
alpha -> 82 : 82.
alpha -> 83 : 83.
alpha -> 84 : 84.
alpha -> 85 : 85.
alpha -> 86 : 86.
alpha -> 87 : 87.
alpha -> 88 : 88.
alpha -> 89 : 89.

% a - z
alpha -> 97 : 97.
alpha -> 98 : 98.
alpha -> 99 : 99.
alpha -> 100 : 100.
alpha -> 101 : 101.
alpha -> 102 : 102.
alpha -> 103 : 103.
alpha -> 104 : 104.
alpha -> 105 : 105.
alpha -> 106 : 106.
alpha -> 107 : 107.
alpha -> 108 : 108.
alpha -> 109 : 109.
alpha -> 110 : 110.
alpha -> 111 : 111.
alpha -> 112 : 112.
alpha -> 113 : 113.
alpha -> 114 : 114.
alpha -> 115 : 115.
alpha -> 116 : 116.
alpha -> 117 : 117.
alpha -> 118 : 118.
alpha -> 119 : 119.
alpha -> 120 : 120.
alpha -> 121 : 121.

bit -> 48 : 48.
bit -> 49 : 49.

char -> 1 : 1.
char -> 2 : 2.
char -> 3 : 3.
char -> 4 : 4.
char -> 5 : 5.
char -> 6 : 6.
char -> 7 : 7.
char -> 8 : 8.
char -> 9 : 9.
char -> 10 : 10.
char -> 11 : 11.
char -> 12 : 12.
char -> 13 : 13.
char -> 14 : 14.
char -> 15 : 15.
char -> 16 : 16.
char -> 17 : 17.
char -> 18 : 18.
char -> 19 : 19.
char -> 20 : 20.
char -> 21 : 21.
char -> 22 : 22.
char -> 23 : 23.
char -> 24 : 24.
char -> 25 : 25.
char -> 26 : 26.
char -> 27 : 27.
char -> 28 : 28.
char -> 29 : 29.
char -> 30 : 30.
char -> 31 : 31.
char -> 32 : 32.
char -> 33 : 33.
char -> 34 : 34.
char -> 35 : 35.
char -> 36 : 36.
char -> 37 : 37.
char -> 38 : 38.
char -> 39 : 39.
char -> 40 : 40.
char -> 41 : 41.
char -> 42 : 42.
char -> 43 : 43.
char -> 44 : 44.
char -> 45 : 45.
char -> 46 : 46.
char -> 47 : 47.
char -> 48 : 48.
char -> 49 : 49.
char -> 50 : 50.
char -> 51 : 51.
char -> 52 : 52.
char -> 53 : 53.
char -> 54 : 54.
char -> 55 : 55.
char -> 56 : 56.
char -> 57 : 57.
char -> 58 : 58.
char -> 59 : 59.
char -> 60 : 60.
char -> 61 : 61.
char -> 62 : 62.
char -> 63 : 63.
char -> 64 : 64.
char -> 65 : 65.
char -> 66 : 66.
char -> 67 : 67.
char -> 68 : 68.
char -> 69 : 69.
char -> 70 : 70.
char -> 71 : 71.
char -> 72 : 72.
char -> 73 : 73.
char -> 74 : 74.
char -> 75 : 75.
char -> 76 : 76.
char -> 77 : 77.
char -> 78 : 78.
char -> 79 : 79.
char -> 80 : 80.
char -> 81 : 81.
char -> 82 : 82.
char -> 83 : 83.
char -> 84 : 84.
char -> 85 : 85.
char -> 86 : 86.
char -> 87 : 87.
char -> 88 : 88.
char -> 89 : 89.
char -> 90 : 90.
char -> 91 : 91.
char -> 92 : 92.
char -> 93 : 93.
char -> 94 : 94.
char -> 95 : 95.
char -> 96 : 96.
char -> 97 : 97.
char -> 98 : 98.
char -> 99 : 99.
char -> 100 : 100.
char -> 101 : 101.
char -> 102 : 102.
char -> 103 : 103.
char -> 104 : 104.
char -> 105 : 105.
char -> 106 : 106.
char -> 107 : 107.
char -> 108 : 108.
char -> 109 : 109.
char -> 110 : 110.
char -> 111 : 111.
char -> 112 : 112.
char -> 113 : 113.
char -> 114 : 114.
char -> 115 : 115.
char -> 116 : 116.
char -> 117 : 117.
char -> 118 : 118.
char -> 119 : 119.
char -> 120 : 120.
char -> 121 : 121.
char -> 122 : 122.
char -> 123 : 123.
char -> 124 : 124.
char -> 125 : 125.
char -> 126 : 126.
char -> 127 : 127.

cr -> 13 : 13.

lf -> 10 : 10.

crlf -> cr lf : 'crlf'.

ctl -> 0 : 0.
ctl -> 1 : 1.
ctl -> 2 : 2.
ctl -> 3 : 3.
ctl -> 4 : 4.
ctl -> 5 : 5.
ctl -> 6 : 6.
ctl -> 7 : 7.
ctl -> 8 : 8.
ctl -> 9 : 9.
ctl -> 10 : 10.
ctl -> 11 : 11.
ctl -> 12 : 12.
ctl -> 13 : 13.
ctl -> 14 : 14.
ctl -> 15 : 15.
ctl -> 16 : 16.
ctl -> 17 : 17.
ctl -> 18 : 18.
ctl -> 19 : 19.
ctl -> 20 : 20.
ctl -> 21 : 21.
ctl -> 22 : 22.
ctl -> 23 : 23.
ctl -> 24 : 24.
ctl -> 25 : 25.
ctl -> 26 : 26.
ctl -> 27 : 27.
ctl -> 28 : 28.
ctl -> 29 : 29.
ctl -> 30 : 30.
ctl -> 127 : 127.

digit -> 48 : 48.
digit -> 49 : 49.
digit -> 50 : 50.
digit -> 51 : 51.
digit -> 52 : 52.
digit -> 53 : 53.
digit -> 54 : 54.
digit -> 55 : 55.
digit -> 56 : 56.
digit -> 57 : 57.

dquote -> 34 : 34.

hexdig -> digit : '$1'.
hexdig -> 65 : 65.
hexdig -> 66 : 66.
hexdig -> 67 : 67.
hexdig -> 68 : 68.
hexdig -> 69 : 69.
hexdig -> 70 : 70.

htab -> 9 : 9.

sp -> 32 : 32.

wsp -> sp : '$1'.
wsp -> htab : '$1'.

anywsp -> wsp : '$1'.
anywsp -> crlf : '$1'.
lwsp -> anywsp : ['$1'].
lwsp -> anywsp lwsp : ['$1' | '$2'].

octet -> 0 : 0.
octet -> 1 : 1.
octet -> 2 : 2.
octet -> 3 : 3.
octet -> 4 : 4.
octet -> 5 : 5.
octet -> 6 : 6.
octet -> 7 : 7.
octet -> 8 : 8.
octet -> 9 : 9.
octet -> 10 : 10.
octet -> 11 : 11.
octet -> 12 : 12.
octet -> 13 : 13.
octet -> 14 : 14.
octet -> 15 : 15.
octet -> 16 : 16.
octet -> 17 : 17.
octet -> 18 : 18.
octet -> 19 : 19.
octet -> 20 : 20.
octet -> 21 : 21.
octet -> 22 : 22.
octet -> 23 : 23.
octet -> 24 : 24.
octet -> 25 : 25.
octet -> 26 : 26.
octet -> 27 : 27.
octet -> 28 : 28.
octet -> 29 : 29.
octet -> 30 : 30.
octet -> 31 : 31.
octet -> 32 : 32.
octet -> 33 : 33.
octet -> 34 : 34.
octet -> 35 : 35.
octet -> 36 : 36.
octet -> 37 : 37.
octet -> 38 : 38.
octet -> 39 : 39.
octet -> 40 : 40.
octet -> 41 : 41.
octet -> 42 : 42.
octet -> 43 : 43.
octet -> 44 : 44.
octet -> 45 : 45.
octet -> 46 : 46.
octet -> 47 : 47.
octet -> 48 : 48.
octet -> 49 : 49.
octet -> 50 : 50.
octet -> 51 : 51.
octet -> 52 : 52.
octet -> 53 : 53.
octet -> 54 : 54.
octet -> 55 : 55.
octet -> 56 : 56.
octet -> 57 : 57.
octet -> 58 : 58.
octet -> 59 : 59.
octet -> 60 : 60.
octet -> 61 : 61.
octet -> 62 : 62.
octet -> 63 : 63.
octet -> 64 : 64.
octet -> 65 : 65.
octet -> 66 : 66.
octet -> 67 : 67.
octet -> 68 : 68.
octet -> 69 : 69.
octet -> 70 : 70.
octet -> 71 : 71.
octet -> 72 : 72.
octet -> 73 : 73.
octet -> 74 : 74.
octet -> 75 : 75.
octet -> 76 : 76.
octet -> 77 : 77.
octet -> 78 : 78.
octet -> 79 : 79.
octet -> 80 : 80.
octet -> 81 : 81.
octet -> 82 : 82.
octet -> 83 : 83.
octet -> 84 : 84.
octet -> 85 : 85.
octet -> 86 : 86.
octet -> 87 : 87.
octet -> 88 : 88.
octet -> 89 : 89.
octet -> 90 : 90.
octet -> 91 : 91.
octet -> 92 : 92.
octet -> 93 : 93.
octet -> 94 : 94.
octet -> 95 : 95.
octet -> 96 : 96.
octet -> 97 : 97.
octet -> 98 : 98.
octet -> 99 : 99.
octet -> 100 : 100.
octet -> 101 : 101.
octet -> 102 : 102.
octet -> 103 : 103.
octet -> 104 : 104.
octet -> 105 : 105.
octet -> 106 : 106.
octet -> 107 : 107.
octet -> 108 : 108.
octet -> 109 : 109.
octet -> 110 : 110.
octet -> 111 : 111.
octet -> 112 : 112.
octet -> 113 : 113.
octet -> 114 : 114.
octet -> 115 : 115.
octet -> 116 : 116.
octet -> 117 : 117.
octet -> 118 : 118.
octet -> 119 : 119.
octet -> 120 : 120.
octet -> 121 : 121.
octet -> 122 : 122.
octet -> 123 : 123.
octet -> 124 : 124.
octet -> 125 : 125.
octet -> 126 : 126.
octet -> 127 : 127.
octet -> 128 : 128.
octet -> 129 : 129.
octet -> 130 : 130.
octet -> 131 : 131.
octet -> 132 : 132.
octet -> 133 : 133.
octet -> 134 : 134.
octet -> 135 : 135.
octet -> 136 : 136.
octet -> 137 : 137.
octet -> 138 : 138.
octet -> 139 : 139.
octet -> 140 : 140.
octet -> 141 : 141.
octet -> 142 : 142.
octet -> 143 : 143.
octet -> 144 : 144.
octet -> 145 : 145.
octet -> 146 : 146.
octet -> 147 : 147.
octet -> 148 : 148.
octet -> 149 : 149.
octet -> 150 : 150.
octet -> 151 : 151.
octet -> 152 : 152.
octet -> 153 : 153.
octet -> 154 : 154.
octet -> 155 : 155.
octet -> 156 : 156.
octet -> 157 : 157.
octet -> 158 : 158.
octet -> 159 : 159.
octet -> 160 : 160.
octet -> 161 : 161.
octet -> 162 : 162.
octet -> 163 : 163.
octet -> 164 : 164.
octet -> 165 : 165.
octet -> 166 : 166.
octet -> 167 : 167.
octet -> 168 : 168.
octet -> 169 : 169.
octet -> 170 : 170.
octet -> 171 : 171.
octet -> 172 : 172.
octet -> 173 : 173.
octet -> 174 : 174.
octet -> 175 : 175.
octet -> 176 : 176.
octet -> 177 : 177.
octet -> 178 : 178.
octet -> 179 : 179.
octet -> 180 : 180.
octet -> 181 : 181.
octet -> 182 : 182.
octet -> 183 : 183.
octet -> 184 : 184.
octet -> 185 : 185.
octet -> 186 : 186.
octet -> 187 : 187.
octet -> 188 : 188.
octet -> 189 : 189.
octet -> 190 : 190.
octet -> 191 : 191.
octet -> 192 : 192.
octet -> 193 : 193.
octet -> 194 : 194.
octet -> 195 : 195.
octet -> 196 : 196.
octet -> 197 : 197.
octet -> 198 : 198.
octet -> 199 : 199.
octet -> 200 : 200.
octet -> 201 : 201.
octet -> 202 : 202.
octet -> 203 : 203.
octet -> 204 : 204.
octet -> 205 : 205.
octet -> 206 : 206.
octet -> 207 : 207.
octet -> 208 : 208.
octet -> 209 : 209.
octet -> 210 : 210.
octet -> 211 : 211.
octet -> 212 : 212.
octet -> 213 : 213.
octet -> 214 : 214.
octet -> 215 : 215.
octet -> 216 : 216.
octet -> 217 : 217.
octet -> 218 : 218.
octet -> 219 : 219.
octet -> 220 : 220.
octet -> 221 : 221.
octet -> 222 : 222.
octet -> 223 : 223.
octet -> 224 : 224.
octet -> 225 : 225.
octet -> 226 : 226.
octet -> 227 : 227.
octet -> 228 : 228.
octet -> 229 : 229.
octet -> 230 : 230.
octet -> 231 : 231.
octet -> 232 : 232.
octet -> 233 : 233.
octet -> 234 : 234.
octet -> 235 : 235.
octet -> 236 : 236.
octet -> 237 : 237.
octet -> 238 : 238.
octet -> 239 : 239.
octet -> 240 : 240.
octet -> 241 : 241.
octet -> 242 : 242.
octet -> 243 : 243.
octet -> 244 : 244.
octet -> 245 : 245.
octet -> 246 : 246.
octet -> 247 : 247.
octet -> 248 : 248.
octet -> 249 : 249.
octet -> 250 : 250.
octet -> 251 : 251.
octet -> 252 : 252.
octet -> 253 : 253.
octet -> 254 : 254.
octet -> 255 : 255.

vchar -> 33 : 33.
vchar -> 34 : 34.
vchar -> 35 : 35.
vchar -> 36 : 36.
vchar -> 37 : 37.
vchar -> 38 : 38.
vchar -> 39 : 39.
vchar -> 40 : 40.
vchar -> 41 : 41.
vchar -> 42 : 42.
vchar -> 43 : 43.
vchar -> 44 : 44.
vchar -> 45 : 45.
vchar -> 46 : 46.
vchar -> 47 : 47.
vchar -> 48 : 48.
vchar -> 49 : 49.
vchar -> 50 : 50.
vchar -> 51 : 51.
vchar -> 52 : 52.
vchar -> 53 : 53.
vchar -> 54 : 54.
vchar -> 55 : 55.
vchar -> 56 : 56.
vchar -> 57 : 57.
vchar -> 58 : 58.
vchar -> 59 : 59.
vchar -> 60 : 60.
vchar -> 61 : 61.
vchar -> 62 : 62.
vchar -> 63 : 63.
vchar -> 64 : 64.
vchar -> 65 : 65.
vchar -> 66 : 66.
vchar -> 67 : 67.
vchar -> 68 : 68.
vchar -> 69 : 69.
vchar -> 70 : 70.
vchar -> 71 : 71.
vchar -> 72 : 72.
vchar -> 73 : 73.
vchar -> 74 : 74.
vchar -> 75 : 75.
vchar -> 76 : 76.
vchar -> 77 : 77.
vchar -> 78 : 78.
vchar -> 79 : 79.
vchar -> 80 : 80.
vchar -> 81 : 81.
vchar -> 82 : 82.
vchar -> 83 : 83.
vchar -> 84 : 84.
vchar -> 85 : 85.
vchar -> 86 : 86.
vchar -> 87 : 87.
vchar -> 88 : 88.
vchar -> 89 : 89.
vchar -> 90 : 90.
vchar -> 91 : 91.
vchar -> 92 : 92.
vchar -> 93 : 93.
vchar -> 94 : 94.
vchar -> 95 : 95.
vchar -> 96 : 96.
vchar -> 97 : 97.
vchar -> 98 : 98.
vchar -> 99 : 99.
vchar -> 100 : 100.
vchar -> 101 : 101.
vchar -> 102 : 102.
vchar -> 103 : 103.
vchar -> 104 : 104.
vchar -> 105 : 105.
vchar -> 106 : 106.
vchar -> 107 : 107.
vchar -> 108 : 108.
vchar -> 109 : 109.
vchar -> 110 : 110.
vchar -> 111 : 111.
vchar -> 112 : 112.
vchar -> 113 : 113.
vchar -> 114 : 114.
vchar -> 115 : 115.
vchar -> 116 : 116.
vchar -> 117 : 117.
vchar -> 118 : 118.
vchar -> 119 : 119.
vchar -> 120 : 120.
vchar -> 121 : 121.
vchar -> 122 : 122.
vchar -> 123 : 123.
vchar -> 124 : 124.
vchar -> 125 : 125.
vchar -> 126 : 126.

Erlang code.
hexval(X) -> erlang:list_to_integer(X, 16).
decval(X) -> list_to_integer(X).
binval(X) -> erlang:list_to_integer(X, 2).

