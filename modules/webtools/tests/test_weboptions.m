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
assert_isequal(nargin('weboptions'), -1);
assert_isequal(nargout('weboptions'), 1);
%=============================================================================
certificateFilename = [modulepath('webtools'), '/resources/cacert.pem'];
if ~isfile(certificateFilename)
  certificateFilename = [];
end
options = weboptions();
assert_isequal(class(options), 'weboptions');
%=============================================================================
st = struct(options);
assert_isequal(st.CharacterEncoding, 'auto');
assert_isequal(st.Timeout, 5);
assert_isequal(st.Username, '');
assert_isequal(st.Password, '');
assert_isequal(st.KeyName, '');
assert_isequal(st.KeyValue, '');
assert_isequal(st.ContentType, 'auto');
assert_isequal(st.ContentReader, []);
assert_isequal(st.MediaType, 'auto');
assert_isequal(st.RequestMethod, 'auto');
assert_isequal(st.ArrayFormat, 'csv');
assert_isequal(st.HeaderFields, []);
assert_isequal(st.CertificateFilename, certificateFilename);
%=============================================================================
options = weboptions('Password', 'Nelson');
st = struct(options);
assert_isequal(st.CharacterEncoding, 'auto');
assert_isequal(st.Timeout, 5);
assert_isequal(st.Username, '');
assert_isequal(st.Password, 'Nelson');
assert_isequal(st.KeyName, '');
assert_isequal(st.KeyValue, '');
assert_isequal(st.ContentType, 'auto');
assert_isequal(st.ContentReader, []);
assert_isequal(st.MediaType, 'auto');
assert_isequal(st.RequestMethod, 'auto');
assert_isequal(st.ArrayFormat, 'csv');
assert_isequal(st.HeaderFields, []);
assert_isequal(st.CertificateFilename, certificateFilename);
%=============================================================================
assert_checkerror('weboptions(''Password1'', ''Nelson'');',sprintf(_('%s is not a recognized parameter.'), 'Password1'))