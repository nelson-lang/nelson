%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('weboptions'), -1);
asserts.isequal(nargout('weboptions'), 1);
%=============================================================================
options = weboptions();
asserts.isequal(class(options), 'weboptions');
%=============================================================================
expectedProperties = {'CharacterEncoding';
'UserAgent';
'Timeout';
'Username';
'Password';
'KeyName';
'KeyValue';
'ContentType';
'ContentReader';
'MediaType';
'RequestMethod';
'ArrayFormat';
'HeaderFields';
'CertificateFilename';
'FollowLocation'};
asserts.isequal(properties(options), expectedProperties);
asserts.isequal(fieldnames(options), expectedProperties);
%=============================================================================
st = struct(options);
asserts.isequal(st.CharacterEncoding, 'auto');
asserts.isequal(st.Timeout, 5);
asserts.isequal(st.Username, '');
asserts.isequal(st.Password, '');
asserts.isequal(st.KeyName, '');
asserts.isequal(st.KeyValue, '');
asserts.isequal(st.ContentType, 'auto');
asserts.isequal(st.ContentReader, []);
asserts.isequal(st.MediaType, 'auto');
asserts.isequal(st.RequestMethod, 'auto');
asserts.isequal(st.ArrayFormat, 'csv');
asserts.isequal(st.HeaderFields, []);
asserts.isequal(st.CertificateFilename, 'default');
asserts.isequal(st.FollowLocation, true);
%=============================================================================
options = weboptions('Password', 'Nelson', 'Time', Inf, 'ContentT', 'image', ...
  'ArrayF', 'php', 'RequestM', 'POST', 'HeaderFields', {'A', 'B'; 'C', 'D'}, ...
  'FollowLocation', true);
asserts.isequal(options.Password, 'Nelson');
asserts.isequal(options.Timeout, Inf);
asserts.isequal(options.ContentType, 'image');
asserts.isequal(options.ArrayFormat, 'php');
asserts.isequal(options.RequestMethod, 'post');
asserts.isequal(options.HeaderFields, {'A', 'B'; 'C', 'D'});
asserts.isequal(options.FollowLocation, true);
%=============================================================================
options = weboptions();
options.ContentType = 'table';
asserts.isequal(options.ContentType, 'table');
options.ContentType = 'xmldom';
asserts.isequal(options.ContentType, 'xmldom');
options.ContentType = 'RAW';
asserts.isequal(options.ContentType, 'raw');
options.Timeout = Inf;
asserts.isequal(options.Timeout, Inf);
options.UserAgent = '';
asserts.istrue(isempty(options.UserAgent));
options.Username = '';
asserts.istrue(isempty(options.Username));
options.Password = '';
asserts.istrue(isempty(options.Password));
options.KeyName = '';
asserts.istrue(isempty(options.KeyName));
options.CertificateFilename = '';
asserts.istrue(isempty(options.CertificateFilename));
options.CertificateFilename = 'default';
asserts.isequal(options.CertificateFilename, 'default');
%=============================================================================
certificateFilename = [modulepath('webtools'), '/resources/cacert.pem'];
if isfile(certificateFilename)
  options.CertificateFilename = certificateFilename;
  asserts.isequal(options.CertificateFilename, certificateFilename);
end
%=============================================================================
asserts.checkerror('weboptions(''Password1'', ''Nelson'');', ...
  sprintf(_('"%s" is not a recognized parameter.'), 'Password1'));
asserts.checkerror('weboptions(''C'', ''text'');', ...
  sprintf(_('"%s" matches multiple parameters.'), 'C'));
asserts.checkerror('weboptions(''Timeout'', 0);', ...
  sprintf(_('%s must be a positive numeric scalar or Inf.'), 'Timeout'));
asserts.checkerror('weboptions(''ContentType'', ''bad'');', ...
  _('Invalid value for "ContentType".'));
asserts.checkerror('weboptions(''HeaderFields'', {''A'', ''B'', ''C''});', ...
  sprintf(_('%s must be an M-by-2 string array or cell array of character vectors.'), 'HeaderFields'));
asserts.checkerror('weboptions(''CertificateFilename'', ''missing-certificate-file.pem'');', ...
  sprintf(_('%s must be "default", empty, or an existing file.'), 'CertificateFilename'));
%=============================================================================
