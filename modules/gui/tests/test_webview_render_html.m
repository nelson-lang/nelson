%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('webview tests require GUI mode.'));
%=============================================================================
asserts.isequal(nargin('__webview_render_html__'), 1);
asserts.isequal(nargout('__webview_render_html__'), 1);
%=============================================================================
html = ['<!doctype html><html><body><div id="nelson-markdowndisp-root">', ...
  '<mjx-container jax="SVG"><svg width="8.699ex" height="2.072ex" ', ...
  'xmlns="http://www.w3.org/2000/svg"><path d="M0 0H10V10H0Z"></path></svg></mjx-container>', ...
  '</div><script>window.nelsonMarkdownReady=false;', ...
  'setTimeout(function(){window.nelsonMarkdownReady=true;},100);</script></body></html>'];
try
  rendered = __webview_render_html__(html);
catch ex
  skip_testsuite(contains(ex.message, _('HTML rendering is not available on this platform.')), ex.message);
  rethrow(ex);
end
asserts.istrue(contains(rendered, '<img'));
asserts.istrue(contains(rendered, 'width="70"'));
asserts.istrue(contains(rendered, 'height="17"'));
asserts.istrue(~contains(rendered, '<mjx-container'));
%=============================================================================
