%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ollamaPrompt(varargin)
   narginchk(1, 3);
    nargoutchk(0, 2);

    PROMPT = varargin{1};
    mustBeTextScalar(PROMPT, 1);
    PROMPT = convertStringsToChars(PROMPT);

    MODEL = 'llama3';
    if nargin > 1
        MODEL = varargin{2};
        mustBeTextScalar(MODEL, 2);
        MODEL = convertStringsToChars(MODEL);
    end

    URL = 'http://127.0.0.1:11434/api/generate';
    if nargin > 2
        URL = varargin{3};
        mustBeTextScalar(URL, 3);
        URL = convertStringsToChars(URL);
    end
    
    st = struct();
    st.model = MODEL;
    st.prompt = PROMPT;
    st.stream = false;
    json = jsonencode(st);
  
    options = weboptions();
    options.RequestMethod = 'POST';
    options.ContentType='json';
    options.MediaType='application/json';
    options.Timeout=120;

    answer = webwrite(URL, json, options);

    varargout{1} = answer.response;
    if nargout > 1
        varargout{1} = answer;
    end
end
%=============================================================================
