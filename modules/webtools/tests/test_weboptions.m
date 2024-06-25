%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
assert_isequal(st.FollowLocation, false);
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
assert_isequal(st.FollowLocation, false);
%=============================================================================
assert_checkerror('weboptions(''Password1'', ''Nelson'');',sprintf(_('%s is not a recognized parameter.'), 'Password1'))
%=============================================================================
