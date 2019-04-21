/*! highlight.js v9.15.6 | BSD3 License | git.io/hljslicense */
!(function(e) {
  var n =
    ("object" == typeof window && window) || ("object" == typeof self && self);
  "undefined" != typeof exports
    ? e(exports)
    : n &&
      ((n.hljs = e({})),
      "function" == typeof define &&
        define.amd &&
        define([], function() {
          return n.hljs;
        }));
})(function(a) {
  var E = [],
    u = Object.keys,
    N = {},
    g = {},
    n = /^(no-?highlight|plain|text)$/i,
    R = /\blang(?:uage)?-([\w-]+)\b/i,
    t = /((^(<[^>]+>|\t|)+|(?:\n)))/gm,
    r = {
      case_insensitive: "cI",
      lexemes: "l",
      contains: "c",
      keywords: "k",
      subLanguage: "sL",
      className: "cN",
      begin: "b",
      beginKeywords: "bK",
      end: "e",
      endsWithParent: "eW",
      illegal: "i",
      excludeBegin: "eB",
      excludeEnd: "eE",
      returnBegin: "rB",
      returnEnd: "rE",
      relevance: "r",
      variants: "v",
      IDENT_RE: "IR",
      UNDERSCORE_IDENT_RE: "UIR",
      NUMBER_RE: "NR",
      C_NUMBER_RE: "CNR",
      BINARY_NUMBER_RE: "BNR",
      RE_STARTERS_RE: "RSR",
      BACKSLASH_ESCAPE: "BE",
      APOS_STRING_MODE: "ASM",
      QUOTE_STRING_MODE: "QSM",
      PHRASAL_WORDS_MODE: "PWM",
      C_LINE_COMMENT_MODE: "CLCM",
      C_BLOCK_COMMENT_MODE: "CBCM",
      HASH_COMMENT_MODE: "HCM",
      NUMBER_MODE: "NM",
      C_NUMBER_MODE: "CNM",
      BINARY_NUMBER_MODE: "BNM",
      CSS_NUMBER_MODE: "CSSNM",
      REGEXP_MODE: "RM",
      TITLE_MODE: "TM",
      UNDERSCORE_TITLE_MODE: "UTM",
      COMMENT: "C",
      beginRe: "bR",
      endRe: "eR",
      illegalRe: "iR",
      lexemesRe: "lR",
      terminators: "t",
      terminator_end: "tE"
    },
    b = "</span>",
    h = {
      classPrefix: "hljs-",
      tabReplace: null,
      useBR: !1,
      languages: void 0
    };
  function _(e) {
    return e
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  }
  function d(e) {
    return e.nodeName.toLowerCase();
  }
  function v(e, n) {
    var t = e && e.exec(n);
    return t && 0 === t.index;
  }
  function p(e) {
    return n.test(e);
  }
  function l(e) {
    var n,
      t = {},
      r = Array.prototype.slice.call(arguments, 1);
    for (n in e) t[n] = e[n];
    return (
      r.forEach(function(e) {
        for (n in e) t[n] = e[n];
      }),
      t
    );
  }
  function M(e) {
    var a = [];
    return (
      (function e(n, t) {
        for (var r = n.firstChild; r; r = r.nextSibling)
          3 === r.nodeType
            ? (t += r.nodeValue.length)
            : 1 === r.nodeType &&
              (a.push({ event: "start", offset: t, node: r }),
              (t = e(r, t)),
              d(r).match(/br|hr|img|input/) ||
                a.push({ event: "stop", offset: t, node: r }));
        return t;
      })(e, 0),
      a
    );
  }
  function i(e) {
    if (r && !e.langApiRestored) {
      for (var n in ((e.langApiRestored = !0), r)) e[n] && (e[r[n]] = e[n]);
      (e.c || []).concat(e.v || []).forEach(i);
    }
  }
  function m(c) {
    function s(e) {
      return (e && e.source) || e;
    }
    function o(e, n) {
      return new RegExp(s(e), "m" + (c.cI ? "i" : "") + (n ? "g" : ""));
    }
    !(function n(t, e) {
      if (!t.compiled) {
        if (((t.compiled = !0), (t.k = t.k || t.bK), t.k)) {
          var r = {},
            a = function(t, e) {
              c.cI && (e = e.toLowerCase()),
                e.split(" ").forEach(function(e) {
                  var n = e.split("|");
                  r[n[0]] = [t, n[1] ? Number(n[1]) : 1];
                });
            };
          "string" == typeof t.k
            ? a("keyword", t.k)
            : u(t.k).forEach(function(e) {
                a(e, t.k[e]);
              }),
            (t.k = r);
        }
        (t.lR = o(t.l || /\w+/, !0)),
          e &&
            (t.bK && (t.b = "\\b(" + t.bK.split(" ").join("|") + ")\\b"),
            t.b || (t.b = /\B|\b/),
            (t.bR = o(t.b)),
            t.endSameAsBegin && (t.e = t.b),
            t.e || t.eW || (t.e = /\B|\b/),
            t.e && (t.eR = o(t.e)),
            (t.tE = s(t.e) || ""),
            t.eW && e.tE && (t.tE += (t.e ? "|" : "") + e.tE)),
          t.i && (t.iR = o(t.i)),
          null == t.r && (t.r = 1),
          t.c || (t.c = []),
          (t.c = Array.prototype.concat.apply(
            [],
            t.c.map(function(e) {
              return (
                (n = "self" === e ? t : e).v && // lgtm [js/use-before-declaration]
                  !n.cached_variants &&
                  (n.cached_variants = n.v.map(function(e) {
                    return l(n, { v: null }, e);
                  })),
                n.cached_variants || (n.eW && [l(n)]) || [n]
              );
              var n; // lgtm [js/unreachable-statement]
            })
          )),
          t.c.forEach(function(e) {
            n(e, t);
          }),
          t.starts && n(t.starts, e);
        var i = t.c
          .map(function(e) {
            return e.bK ? "\\.?(?:" + e.b + ")\\.?" : e.b;
          })
          .concat([t.tE, t.i])
          .map(s)
          .filter(Boolean);
        t.t = i.length
          ? o(
              (function(e, n) {
                for (
                  var t = /\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./,
                    r = 0,
                    a = "",
                    i = 0;
                  i < e.length;
                  i++
                ) {
                  var c = r,
                    o = s(e[i]);
                  for (0 < i && (a += n); 0 < o.length; ) {
                    var u = t.exec(o);
                    if (null == u) {
                      a += o;
                      break;
                    }
                    (a += o.substring(0, u.index)),
                      (o = o.substring(u.index + u[0].length)),
                      "\\" == u[0][0] && u[1]
                        ? (a += "\\" + String(Number(u[1]) + c))
                        : ((a += u[0]), "(" == u[0] && r++);
                  }
                }
                return a;
              })(i, "|"),
              !0
            )
          : {
              exec: function() {
                return null;
              }
            };
      }
    })(c);
  }
  function C(e, n, o, t) {
    function u(e, n, t, r) {
      var a = '<span class="' + (r ? "" : h.classPrefix);
      return (a += e + '">') + n + (t ? "" : b);
    }
    function s() {
      (g +=
        null != E.sL
          ? (function() {
              var e = "string" == typeof E.sL;
              if (e && !N[E.sL]) return _(R);
              var n = e
                ? C(E.sL, R, !0, i[E.sL])
                : O(R, E.sL.length ? E.sL : void 0);
              return (
                0 < E.r && (d += n.r),
                e && (i[E.sL] = n.top),
                u(n.language, n.value, !1, !0)
              );
            })()
          : (function() {
              var e, n, t, r, a, i, c;
              if (!E.k) return _(R);
              for (r = "", n = 0, E.lR.lastIndex = 0, t = E.lR.exec(R); t; )
                (r += _(R.substring(n, t.index))),
                  (a = E),
                  (i = t),
                  (c = f.cI ? i[0].toLowerCase() : i[0]),
                  (e = a.k.hasOwnProperty(c) && a.k[c])
                    ? ((d += e[1]), (r += u(e[0], _(t[0]))))
                    : (r += _(t[0])),
                  (n = E.lR.lastIndex),
                  (t = E.lR.exec(R));
              return r + _(R.substr(n));
            })()),
        (R = "");
    }
    function l(e) {
      (g += e.cN ? u(e.cN, "", !0) : ""),
        (E = Object.create(e, { parent: { value: E } }));
    }
    function r(e, n) {
      if (((R += e), null == n)) return s(), 0;
      var t = (function(e, n) {
        var t, r, a;
        for (t = 0, r = n.c.length; t < r; t++)
          if (v(n.c[t].bR, e))
            return (
              n.c[t].endSameAsBegin &&
                (n.c[t].eR = ((a = n.c[t].bR.exec(e)[0]),
                new RegExp(a.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"), "m"))),
              n.c[t]
            );
      })(n, E);
      if (t)
        return (
          t.skip ? (R += n) : (t.eB && (R += n), s(), t.rB || t.eB || (R = n)),
          l(t),
          t.rB ? 0 : n.length
        );
      var r,
        a,
        i = (function e(n, t) {
          if (v(n.eR, t)) {
            for (; n.endsParent && n.parent; ) n = n.parent;
            return n;
          }
          if (n.eW) return e(n.parent, t);
        })(E, n);
      if (i) {
        var c = E;
        for (
          c.skip ? (R += n) : (c.rE || c.eE || (R += n), s(), c.eE && (R = n));
          E.cN && (g += b),
            E.skip || E.sL || (d += E.r),
            (E = E.parent) !== i.parent;

        );
        return (
          i.starts && (i.endSameAsBegin && (i.starts.eR = i.eR), l(i.starts)),
          c.rE ? 0 : n.length
        );
      }
      if (((r = n), (a = E), !o && v(a.iR, r)))
        throw new Error(
          'Illegal lexeme "' + n + '" for mode "' + (E.cN || "<unnamed>") + '"'
        );
      return (R += n), n.length || 1;
    }
    var f = S(e);
    if (!f) throw new Error('Unknown language: "' + e + '"');
    m(f);
    var a,
      E = t || f,
      i = {},
      g = "";
    for (a = E; a !== f; a = a.parent) a.cN && (g = u(a.cN, "", !0) + g);
    var R = "",
      d = 0;
    try {
      for (var c, p, M = 0; (E.t.lastIndex = M), (c = E.t.exec(n)); )
        (p = r(n.substring(M, c.index), c[0])), (M = c.index + p);
      for (r(n.substr(M)), a = E; a.parent; a = a.parent) a.cN && (g += b);
      return { r: d, value: g, language: e, top: E };
    } catch (e) {
      if (e.message && -1 !== e.message.indexOf("Illegal"))
        return { r: 0, value: _(n) };
      throw e;
    }
  }
  function O(t, e) {
    e = e || h.languages || u(N);
    var r = { r: 0, value: _(t) },
      a = r;
    return (
      e
        .filter(S)
        .filter(s)
        .forEach(function(e) {
          var n = C(e, t, !1);
          (n.language = e),
            n.r > a.r && (a = n),
            n.r > r.r && ((a = r), (r = n));
        }),
      a.language && (r.second_best = a),
      r
    );
  }
  function B(e) {
    return h.tabReplace || h.useBR
      ? e.replace(t, function(e, n) {
          return h.useBR && "\n" === e
            ? "<br>"
            : h.tabReplace
            ? n.replace(/\t/g, h.tabReplace)
            : "";
        })
      : e;
  }
  function c(e) {
    var n,
      t,
      r,
      a,
      i,
      c,
      o,
      u,
      s,
      l,
      f = (function(e) {
        var n,
          t,
          r,
          a,
          i = e.className + " ";
        if (
          ((i += e.parentNode ? e.parentNode.className : ""), (t = R.exec(i)))
        )
          return S(t[1]) ? t[1] : "no-highlight";
        for (n = 0, r = (i = i.split(/\s+/)).length; n < r; n++)
          if (p((a = i[n])) || S(a)) return a;
      })(e);
    p(f) ||
      (h.useBR
        ? ((n = document.createElementNS(
            "http://www.w3.org/1999/xhtml",
            "div"
          )).innerHTML = e.innerHTML
            .replace(/\n/g, "")
            .replace(/<br[ \/]*>/g, "\n"))
        : (n = e),
      (i = n.textContent),
      (r = f ? C(f, i, !0) : O(i)),
      (t = M(n)).length &&
        (((a = document.createElementNS(
          "http://www.w3.org/1999/xhtml",
          "div"
        )).innerHTML = r.value),
        (r.value = (function(e, n, t) {
          var r = 0,
            a = "",
            i = [];
          function c() {
            return e.length && n.length
              ? e[0].offset !== n[0].offset
                ? e[0].offset < n[0].offset
                  ? e
                  : n
                : "start" === n[0].event
                ? e
                : n
              : e.length
              ? e
              : n;
          }
          function o(e) {
            a +=
              "<" +
              d(e) +
              E.map
                .call(e.attributes, function(e) {
                  return (
                    " " +
                    e.nodeName +
                    '="' +
                    _(e.value).replace('"', "&quot;") + // lgtm [js/incomplete-sanitization]
                    '"'
                  );
                })
                .join("") +
              ">";
          }
          function u(e) {
            a += "</" + d(e) + ">";
          }
          function s(e) {
            ("start" === e.event ? o : u)(e.node);
          }
          for (; e.length || n.length; ) {
            var l = c();
            if (
              ((a += _(t.substring(r, l[0].offset))),
              (r = l[0].offset),
              l === e)
            ) {
              for (
                i.reverse().forEach(u);
                s(l.splice(0, 1)[0]),
                  (l = c()) === e && l.length && l[0].offset === r;

              );
              i.reverse().forEach(o);
            } else
              "start" === l[0].event ? i.push(l[0].node) : i.pop(),
                s(l.splice(0, 1)[0]);
          }
          return a + _(t.substr(r));
        })(t, M(a), i))),
      (r.value = B(r.value)),
      (e.innerHTML = r.value),
      (e.className = ((c = e.className),
      (o = f),
      (u = r.language),
      (s = o ? g[o] : u),
      (l = [c.trim()]),
      c.match(/\bhljs\b/) || l.push("hljs"),
      -1 === c.indexOf(s) && l.push(s),
      l.join(" ").trim())),
      (e.result = { language: r.language, re: r.r }),
      r.second_best &&
        (e.second_best = {
          language: r.second_best.language,
          re: r.second_best.r
        }));
  }
  function o() {
    if (!o.called) {
      o.called = !0;
      var e = document.querySelectorAll("pre code");
      E.forEach.call(e, c);
    }
  }
  function S(e) {
    return (e = (e || "").toLowerCase()), N[e] || N[g[e]];
  }
  function s(e) {
    var n = S(e);
    return n && !n.disableAutodetect;
  }
  return (
    (a.highlight = C),
    (a.highlightAuto = O),
    (a.fixMarkup = B),
    (a.highlightBlock = c),
    (a.configure = function(e) {
      h = l(h, e);
    }),
    (a.initHighlighting = o),
    (a.initHighlightingOnLoad = function() {
      addEventListener("DOMContentLoaded", o, !1),
        addEventListener("load", o, !1);
    }),
    (a.registerLanguage = function(n, e) {
      var t = (N[n] = e(a));
      i(t),
        t.aliases &&
          t.aliases.forEach(function(e) {
            g[e] = n;
          });
    }),
    (a.listLanguages = function() {
      return u(N);
    }),
    (a.getLanguage = S),
    (a.autoDetection = s),
    (a.inherit = l),
    (a.IR = a.IDENT_RE = "[a-zA-Z]\\w*"),
    (a.UIR = a.UNDERSCORE_IDENT_RE = "[a-zA-Z_]\\w*"),
    (a.NR = a.NUMBER_RE = "\\b\\d+(\\.\\d+)?"),
    (a.CNR = a.C_NUMBER_RE =
      "(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)"),
    (a.BNR = a.BINARY_NUMBER_RE = "\\b(0b[01]+)"),
    (a.RSR = a.RE_STARTERS_RE =
      "!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~"),
    (a.BE = a.BACKSLASH_ESCAPE = { b: "\\\\[\\s\\S]", r: 0 }),
    (a.ASM = a.APOS_STRING_MODE = {
      cN: "string",
      b: "'",
      e: "'",
      i: "\\n",
      c: [a.BE]
    }),
    (a.QSM = a.QUOTE_STRING_MODE = {
      cN: "string",
      b: '"',
      e: '"',
      i: "\\n",
      c: [a.BE]
    }),
    (a.PWM = a.PHRASAL_WORDS_MODE = {
      b: /\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/
    }),
    (a.C = a.COMMENT = function(e, n, t) {
      var r = a.inherit({ cN: "comment", b: e, e: n, c: [] }, t || {});
      return (
        r.c.push(a.PWM),
        r.c.push({ cN: "doctag", b: "(?:TODO|FIXME|NOTE|BUG|XXX):", r: 0 }),
        r
      );
    }),
    (a.CLCM = a.C_LINE_COMMENT_MODE = a.C("//", "$")),
    (a.CBCM = a.C_BLOCK_COMMENT_MODE = a.C("/\\*", "\\*/")),
    (a.HCM = a.HASH_COMMENT_MODE = a.C("#", "$")),
    (a.NM = a.NUMBER_MODE = { cN: "number", b: a.NR, r: 0 }),
    (a.CNM = a.C_NUMBER_MODE = { cN: "number", b: a.CNR, r: 0 }),
    (a.BNM = a.BINARY_NUMBER_MODE = { cN: "number", b: a.BNR, r: 0 }),
    (a.CSSNM = a.CSS_NUMBER_MODE = {
      cN: "number",
      b:
        a.NR +
        "(%|em|ex|ch|rem|vw|vh|vmin|vmax|cm|mm|in|pt|pc|px|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx)?",
      r: 0
    }),
    (a.RM = a.REGEXP_MODE = {
      cN: "regexp",
      b: /\//,
      e: /\/[gimuy]*/,
      i: /\n/,
      c: [a.BE, { b: /\[/, e: /\]/, r: 0, c: [a.BE] }]
    }),
    (a.TM = a.TITLE_MODE = { cN: "title", b: a.IR, r: 0 }),
    (a.UTM = a.UNDERSCORE_TITLE_MODE = { cN: "title", b: a.UIR, r: 0 }),
    (a.METHOD_GUARD = { b: "\\.\\s*" + a.UIR, r: 0 }),
    a
  );
});
hljs.registerLanguage("nelson", function(e) {
  var a = "('|\\.')+",
    s = { r: 0, c: [{ b: a }] };
  return {
    k: {
      keyword:
        "break case catch continue else elseif end endfunction for function global if otherwise parfor persistent return switch try while",
      built_in:
        "COM_class COM_delete COM_disp COM_fieldnames COM_get COM_invoke COM_ismethod COM_isprop COM_isvalid COM_methods COM_range COM_set COM_used IJV Inf MPI_Allreduce MPI_Barrier MPI_Bcast MPI_Comm_delete MPI_Comm_disp MPI_Comm_get_name MPI_Comm_isvalid MPI_Comm_object MPI_Comm_rank MPI_Comm_size MPI_Comm_split MPI_Comm_used MPI_Finalize MPI_Get_library_version MPI_Get_processor_name MPI_Get_version MPI_Init MPI_Initialized MPI_Iprobe MPI_Probe MPI_Recv MPI_Reduce MPI_Send NaN QObject_classname QObject_delete QObject_disp QObject_fieldnames QObject_findchildren QObject_get QObject_invoke QObject_ismethod QObject_isprop QObject_isvalid QObject_iswidgettype QObject_iswindowtype QObject_methods QObject_methodsignature QObject_properties QObject_root QObject_set QObject_undefine QObject_used abs acos acquirevar actxGetRunningServer actxcontrollist actxserver actxserverlist addgateway addmodule addpath all and any argv asin assert assert_checkerror assert_isapprox assert_isequal assert_isfalse assert_istrue assignin atan atan2 audiodevinfo audioinfo audiometadata audioplayer audioplayer_delete audioplayer_disp audioplayer_fieldnames audioplayer_get audioplayer_ismethod audioplayer_isprop audioplayer_isvalid audioplayer_pause audioplayer_play audioplayer_playblocking audioplayer_properties audioplayer_resume audioplayer_set audioplayer_stop audioplayer_used audioread audiosupportedformats audiowrite banner banner beep blanks builtin calendar cast cd ceil cell cell2struct cellfun char class clc clear clearfun clock colon complex computer conj contains convertCharsToStrings convertStringsToChars copyfile cos cosh cosm count cputime createGUID ctranspose datenum datevec dbstack deblank delete diary diff_file dir disp dlcall dlclose dllib_delete dllib_disp dllib_fieldnames dllib_get dllib_ismethod dllib_isprop dllib_isvalid dllib_used dllibinfo dlmwrite dlopen dlsym dlsym_delete dlsym_disp dlsym_fieldnames dlsym_get dlsym_ismethod dlsym_isprop dlsym_isvalid dlsym_used dos double echo editor endsWith eps eq error eval evalc evalin execstr exit exp expm eye false fclose feval fft fftw fgetl fgets fieldnames fileparts fileread filesep filewrite fix floor fopen format fprintf fread frewind fseek fsize ftell full func2str function_handle_disp function_handle_extraction function_handle_fieldnames function_handle_isequal function_handle_isequaln function_handle_isequalto fwrite gamma gatewayinfo ge generic_eq_handle get getavailablelanguages getdefaultlanguage getdynlibext getenv getlanguage getmodules getnelsonmode gettext global gt h5create h5read h5readatt h5write h5writeatt handle_delete handle_disp handle_eq_generic handle_eq_handle handle_fieldnames handle_get handle_horzcat_handle handle_invoke handle_isequal handle_isequaln handle_isequalto handle_ismethod handle_isprop handle_isvalid handle_methods handle_properties handle_set handle_vertcat_handle headcomments helpbrowser history history_manager horzcat htmltopdf i ifft imag inf input inserthtml int16 int2str int32 int64 int8 intmax intmin inv invoke isNull isa isapprox isbuiltin iscell iscellstr ischar isclass isdir isdouble isempty isequal isequaln isequalto isfield isfile isfinite isfloat isfolder isfunction_handle isglobal ishandle isinf isint16 isint32 isint64 isint8 isinteger iskeyword islogical ismac ismacro ismatfile ismethod ismissing ismodule isnan isnh5file isnumeric ispc isprop isquietmode isreal issingle issparse isstring isstruct issymmetric isuint16 isuint32 isuint64 isuint8 isunix isvalid isvar j jsondecode jsonencode jsonprettyprint lasterror lastwarn ldivide le length libpointer libpointer_delete libpointer_disp libpointer_fieldnames libpointer_get libpointer_isNull libpointer_ismethod libpointer_isprop libpointer_isvalid libpointer_plus libpointer_reshape libpointer_setdatatype libpointer_used load loadmat loadnh5 log logical logm lower lt macroargs markdown mat2str maxNumCompThreads max_recursion_depth memory methods minus mkdir mldivide mod modulepath mpower mrdivide mtimes namelengthmax nan nargin nargout ndims ne nelsonroot nfilename nnz norm not now numel nzmax ones or overloadbasictypes parsefile parsestring path pathsep pause persistent pi play playblocking plus power prefdir prod profile properties pwd qml_addimportpath qml_addpluginpath qml_clearcomponentcache qml_collectgarbage qml_createqquickview qml_evaluatefile qml_evaluatestring qml_importpathlist qml_loadfile qml_loadstring qml_offlinestoragepath qml_pluginpathlist qml_setofflinestoragepath qt_verbose rand randn rcond rdivide real rehash relativepath rem removegateway removemodule replace repmat requiremodule reshape restoredefaultpath resume rmdir rmfile rmpath rng round run save savemat savenh5 schur searchenv set setenv setlanguage shortcutand shortcutor sin single sinh sinm size sleep slicot_ab01od slicot_ab04md slicot_ab07nd slicot_ab07nd slicot_ab08nd slicot_ag08bd slicot_mb02md slicot_mb03od slicot_mb03pd slicot_mb03rd slicot_mb04gd slicot_mb04md slicot_mb05od slicot_mc01td slicot_sb01bd slicot_sb02od slicot_sb03md slicot_sb03od slicot_sb04md slicot_sb04qd slicot_sb10jd slicot_sg02ad slicot_tb01id slicot_tg01ad smartindent sparse sparsedouble_ctranspose sparsedouble_disp sparsedouble_horzcat_sparsedouble sparsedouble_imag sparsedouble_real sparsedouble_transpose sparsedouble_uminus sparsedouble_vertcat_sparsedouble sparselogical_ctranspose sparselogical_disp sparselogical_horzcat_sparselogical sparselogical_imag sparselogical_real sparselogical_transpose sparselogical_uminus sparselogical_vertcat_sparselogical sprintf sqrt sqrtm startsWith stop str2double str2func strcmp strcmpi strfind string strings strlength strncmp strncmpi strrep strtrim struct struct2cell subsindex svd system tan tanh tanm tempdir test_parsetags tic times toc tolower toupper trace transpose true uigetdir uint16 uint32 uint64 uint8 uminus unix uplus upper userdir userpath varislock varlock varunlock version vertcat warning what which who whomat whonh5 whos whosmat whosnh5 winopen winqueryreg xmldocbuild xmldocchecker xor zeros "
    },
    i: '(//|"|#|/\\*|\\s+/\\w+)',
    c: [
      {
        cN: "function",
        bK: "function",
        e: "$",
        c: [
          e.UTM,
          { cN: "params", v: [{ b: "\\(", e: "\\)" }, { b: "\\[", e: "\\]" }] }
        ]
      },
      { cN: "built_in", b: /true|false/, r: 0, starts: s },
      { b: "[a-zA-Z][a-zA-Z_0-9]*" + a, r: 0 },
      { cN: "number", b: e.CNR, r: 0, starts: s },
      { cN: "string", b: "'", e: "'", c: [e.BE, { b: "''" }] },
      { b: /\]|}|\)/, r: 0, starts: s },
      { cN: "string", b: '"', e: '"', c: [e.BE, { b: '""' }], starts: s },
      e.C("^\\s*\\%\\{\\s*$", "^\\s*\\%\\}\\s*$"),
      e.C("\\%", "//")
    ]
  };
});
