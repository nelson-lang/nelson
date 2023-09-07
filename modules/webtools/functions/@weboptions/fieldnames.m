%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = fieldnames(options)
  names = {'CharacterEncoding';
  'UserAgent';
  'Timeout';
  'Username';
  'Password';
  'KeyName';
  'KeyValue';
  'HeaderFields';
  'ContentType';
  'ContentReader';
  'MediaType';
  'RequestMethod';
  'Arrayformat';
  'CertificateFilename'};
end
