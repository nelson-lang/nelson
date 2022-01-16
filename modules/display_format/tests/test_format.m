%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
format('default');
%=============================================================================
f = format();
assert_isequal(f.NumericFormat, "short") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('short');
f = format();
assert_isequal(f.NumericFormat, "short") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('long');
f = format();
assert_isequal(f.NumericFormat, "long") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('shortE');
f = format();
assert_isequal(f.NumericFormat, "shortE") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('shortG');
f = format();
assert_isequal(f.NumericFormat, "shortG") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('short', 'e');
f = format();
assert_isequal(f.NumericFormat, "shortE") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('longE');
f = format();
assert_isequal(f.NumericFormat, "longE") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('long', 'e');
f = format();
assert_isequal(f.NumericFormat, "longE") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('longG');
f = format();
assert_isequal(f.NumericFormat, "longG") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('long', 'g');
f = format();
assert_isequal(f.NumericFormat, "longG") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('shortEng');
f = format();
assert_isequal(f.NumericFormat, "shortEng") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('short', 'Eng');
f = format();
assert_isequal(f.NumericFormat, "shortEng") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('longEng');
f = format();
assert_isequal(f.NumericFormat, "longEng") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('long', 'Eng');
f = format();
assert_isequal(f.NumericFormat, "longEng") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('+');
f = format();
assert_isequal(f.NumericFormat, "+") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('bank');
f = format();
assert_isequal(f.NumericFormat, "bank") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('hex');
f = format();
assert_isequal(f.NumericFormat, "hex") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('rational');
f = format();
assert_isequal(f.NumericFormat, "rational") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
format('compact');
f = format();
assert_isequal(f.NumericFormat, "rational") 
assert_isequal(f.LineSpacing, "compact")
%=============================================================================
format('loose');
f = format();
assert_isequal(f.NumericFormat, "rational") 
assert_isequal(f.LineSpacing, "loose")
%=============================================================================
