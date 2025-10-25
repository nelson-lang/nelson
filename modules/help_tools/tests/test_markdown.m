%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = markdown('### Hello');
REF = '<h3>Hello</h3>
';
assert_isequal(R, REF);
%=============================================================================
R = markdown('**Hello**');
REF = '<p><strong>Hello</strong></p>
';
assert_isequal(R, REF);
%=============================================================================
md_path = [modulepath('help_tools', 'root'), '/tests/md/readme.md'];
assert_istrue(isfile(md_path));
%=============================================================================
dest_path = [tempdir(), 'md'];
if ~isdir(dest_path)
  mkdir(dest_path, '-s');
end
dest_path = [tempdir(), 'md/readme.html'];
assert_istrue(markdown(md_path, dest_path));
R = fileread(dest_path);
REF =  '<h3>Hello</h3>
<p><strong>World</strong></p>
';
assert_isequal(R, REF);
%=============================================================================
txt = 'Hello <script>alert("XSS")</script> World';
advanced_html = markdown(txt, 'advanced');
REF = '<p>Hello <script>alert(“XSS”)</script> World</p>
';
assert_isequal(advanced_html, REF);
%=============================================================================
secure_html = markdown(txt, 'secure');
REF = '<p>Hello <!-- raw HTML omitted -->alert(&quot;XSS&quot;)<!-- raw HTML omitted --> World</p>
';
assert_isequal(secure_html, REF);
%=============================================================================
