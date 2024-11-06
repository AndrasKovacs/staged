"use strict";
var __getOwnPropNames = Object.getOwnPropertyNames;
var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};

// node_modules/readline-sync/lib/readline-sync.js
var require_readline_sync = __commonJS({
  "node_modules/readline-sync/lib/readline-sync.js"(exports2) {
    "use strict";
    var IS_WIN = process.platform === "win32";
    var ALGORITHM_CIPHER = "aes-256-cbc";
    var ALGORITHM_HASH = "sha256";
    var DEFAULT_ERR_MSG = "The current environment doesn't support interactive reading from TTY.";
    var fs = require("fs");
    var TTY = process.binding("tty_wrap").TTY;
    var childProc = require("child_process");
    var pathUtil = require("path");
    var defaultOptions = {
      /* eslint-disable key-spacing */
      prompt: "> ",
      hideEchoBack: false,
      mask: "*",
      limit: [],
      limitMessage: "Input another, please.$<( [)limit(])>",
      defaultInput: "",
      trueValue: [],
      falseValue: [],
      caseSensitive: false,
      keepWhitespace: false,
      encoding: "utf8",
      bufferSize: 1024,
      print: void 0,
      history: true,
      cd: false,
      phContent: void 0,
      preCheck: void 0
      /* eslint-enable key-spacing */
    };
    var fdR = "none";
    var isRawMode = false;
    var salt = 0;
    var lastInput = "";
    var inputHistory = [];
    var _DBG_useExt = false;
    var _DBG_checkOptions = false;
    var _DBG_checkMethod = false;
    var fdW;
    var ttyR;
    var extHostPath;
    var extHostArgs;
    var tempdir;
    var rawInput;
    function getHostArgs(options) {
      function encodeArg(arg) {
        return arg.replace(/[^\w\u0080-\uFFFF]/g, function(chr) {
          return "#" + chr.charCodeAt(0) + ";";
        });
      }
      return extHostArgs.concat(function(conf) {
        var args = [];
        Object.keys(conf).forEach(function(optionName) {
          if (conf[optionName] === "boolean") {
            if (options[optionName]) {
              args.push("--" + optionName);
            }
          } else if (conf[optionName] === "string") {
            if (options[optionName]) {
              args.push("--" + optionName, encodeArg(options[optionName]));
            }
          }
        });
        return args;
      }({
        /* eslint-disable key-spacing */
        display: "string",
        displayOnly: "boolean",
        keyIn: "boolean",
        hideEchoBack: "boolean",
        mask: "string",
        limit: "string",
        caseSensitive: "boolean"
        /* eslint-enable key-spacing */
      }));
    }
    function _execFileSync(options, execOptions) {
      function getTempfile(name) {
        var suffix = "", filepath, fd;
        tempdir = tempdir || require("os").tmpdir();
        while (true) {
          filepath = pathUtil.join(tempdir, name + suffix);
          try {
            fd = fs.openSync(filepath, "wx");
          } catch (e) {
            if (e.code === "EEXIST") {
              suffix++;
              continue;
            } else {
              throw e;
            }
          }
          fs.closeSync(fd);
          break;
        }
        return filepath;
      }
      var res = {}, pathStdout = getTempfile("readline-sync.stdout"), pathStderr = getTempfile("readline-sync.stderr"), pathExit = getTempfile("readline-sync.exit"), pathDone = getTempfile("readline-sync.done"), crypto = require("crypto"), hostArgs, shellPath, shellArgs, exitCode, extMessage, shasum, decipher, password;
      shasum = crypto.createHash(ALGORITHM_HASH);
      shasum.update("" + process.pid + salt++ + Math.random());
      password = shasum.digest("hex");
      decipher = crypto.createDecipher(ALGORITHM_CIPHER, password);
      hostArgs = getHostArgs(options);
      if (IS_WIN) {
        shellPath = process.env.ComSpec || "cmd.exe";
        process.env.Q = '"';
        shellArgs = [
          "/V:ON",
          "/S",
          "/C",
          "(%Q%" + shellPath + "%Q% /V:ON /S /C %Q%%Q%" + extHostPath + "%Q%" + hostArgs.map(function(arg) {
            return " %Q%" + arg + "%Q%";
          }).join("") + " & (echo !ERRORLEVEL!)>%Q%" + pathExit + "%Q%%Q%) 2>%Q%" + pathStderr + "%Q% |%Q%" + process.execPath + "%Q% %Q%" + __dirname + "\\encrypt.js%Q% %Q%" + ALGORITHM_CIPHER + "%Q% %Q%" + password + "%Q% >%Q%" + pathStdout + "%Q% & (echo 1)>%Q%" + pathDone + "%Q%"
        ];
      } else {
        shellPath = "/bin/sh";
        shellArgs = [
          "-c",
          // Use `()`, not `{}` for `-c` (text param)
          '("' + extHostPath + '"' + /* ESLint bug? */
          // eslint-disable-line no-path-concat
          hostArgs.map(function(arg) {
            return " '" + arg.replace(/'/g, "'\\''") + "'";
          }).join("") + '; echo $?>"' + pathExit + '") 2>"' + pathStderr + '" |"' + process.execPath + '" "' + __dirname + '/encrypt.js" "' + ALGORITHM_CIPHER + '" "' + password + '" >"' + pathStdout + '"; echo 1 >"' + pathDone + '"'
        ];
      }
      if (_DBG_checkMethod) {
        _DBG_checkMethod("_execFileSync", hostArgs);
      }
      try {
        childProc.spawn(shellPath, shellArgs, execOptions);
      } catch (e) {
        res.error = new Error(e.message);
        res.error.method = "_execFileSync - spawn";
        res.error.program = shellPath;
        res.error.args = shellArgs;
      }
      while (fs.readFileSync(pathDone, { encoding: options.encoding }).trim() !== "1") {
      }
      if ((exitCode = fs.readFileSync(pathExit, { encoding: options.encoding }).trim()) === "0") {
        res.input = decipher.update(
          fs.readFileSync(pathStdout, { encoding: "binary" }),
          "hex",
          options.encoding
        ) + decipher.final(options.encoding);
      } else {
        extMessage = fs.readFileSync(pathStderr, { encoding: options.encoding }).trim();
        res.error = new Error(DEFAULT_ERR_MSG + (extMessage ? "\n" + extMessage : ""));
        res.error.method = "_execFileSync";
        res.error.program = shellPath;
        res.error.args = shellArgs;
        res.error.extMessage = extMessage;
        res.error.exitCode = +exitCode;
      }
      fs.unlinkSync(pathStdout);
      fs.unlinkSync(pathStderr);
      fs.unlinkSync(pathExit);
      fs.unlinkSync(pathDone);
      return res;
    }
    function readlineExt(options) {
      var res = {}, execOptions = { env: process.env, encoding: options.encoding }, hostArgs, extMessage;
      if (!extHostPath) {
        if (IS_WIN) {
          if (process.env.PSModulePath) {
            extHostPath = "powershell.exe";
            extHostArgs = [
              "-ExecutionPolicy",
              "Bypass",
              "-File",
              __dirname + "\\read.ps1"
            ];
          } else {
            extHostPath = "cscript.exe";
            extHostArgs = ["//nologo", __dirname + "\\read.cs.js"];
          }
        } else {
          extHostPath = "/bin/sh";
          extHostArgs = [__dirname + "/read.sh"];
        }
      }
      if (IS_WIN && !process.env.PSModulePath) {
        execOptions.stdio = [process.stdin];
      }
      if (childProc.execFileSync) {
        hostArgs = getHostArgs(options);
        if (_DBG_checkMethod) {
          _DBG_checkMethod("execFileSync", hostArgs);
        }
        try {
          res.input = childProc.execFileSync(extHostPath, hostArgs, execOptions);
        } catch (e) {
          extMessage = e.stderr ? (e.stderr + "").trim() : "";
          res.error = new Error(DEFAULT_ERR_MSG + (extMessage ? "\n" + extMessage : ""));
          res.error.method = "execFileSync";
          res.error.program = extHostPath;
          res.error.args = hostArgs;
          res.error.extMessage = extMessage;
          res.error.exitCode = e.status;
          res.error.code = e.code;
          res.error.signal = e.signal;
        }
      } else {
        res = _execFileSync(options, execOptions);
      }
      if (!res.error) {
        res.input = res.input.replace(/^\s*'|'\s*$/g, "");
        options.display = "";
      }
      return res;
    }
    function _readlineSync(options) {
      var input = "", displaySave = options.display, silent = !options.display && options.keyIn && options.hideEchoBack && !options.mask;
      function tryExt() {
        var res = readlineExt(options);
        if (res.error) {
          throw res.error;
        }
        return res.input;
      }
      if (_DBG_checkOptions) {
        _DBG_checkOptions(options);
      }
      (function() {
        var fsB, constants, verNum;
        function getFsB() {
          if (!fsB) {
            fsB = process.binding("fs");
            constants = process.binding("constants");
            constants = constants && constants.fs && typeof constants.fs.O_RDWR === "number" ? constants.fs : constants;
          }
          return fsB;
        }
        if (typeof fdR !== "string") {
          return;
        }
        fdR = null;
        if (IS_WIN) {
          verNum = function(ver) {
            var nums = ver.replace(/^\D+/, "").split(".");
            var verNum2 = 0;
            if (nums[0] = +nums[0]) {
              verNum2 += nums[0] * 1e4;
            }
            if (nums[1] = +nums[1]) {
              verNum2 += nums[1] * 100;
            }
            if (nums[2] = +nums[2]) {
              verNum2 += nums[2];
            }
            return verNum2;
          }(process.version);
          if (!(verNum >= 20302 && verNum < 40204 || verNum >= 5e4 && verNum < 50100 || verNum >= 50600 && verNum < 60200) && process.stdin.isTTY) {
            process.stdin.pause();
            fdR = process.stdin.fd;
            ttyR = process.stdin._handle;
          } else {
            try {
              fdR = getFsB().open("CONIN$", constants.O_RDWR, parseInt("0666", 8));
              ttyR = new TTY(fdR, true);
            } catch (e) {
            }
          }
          if (process.stdout.isTTY) {
            fdW = process.stdout.fd;
          } else {
            try {
              fdW = fs.openSync("\\\\.\\CON", "w");
            } catch (e) {
            }
            if (typeof fdW !== "number") {
              try {
                fdW = getFsB().open("CONOUT$", constants.O_RDWR, parseInt("0666", 8));
              } catch (e) {
              }
            }
          }
        } else {
          if (process.stdin.isTTY) {
            process.stdin.pause();
            try {
              fdR = fs.openSync("/dev/tty", "r");
              ttyR = process.stdin._handle;
            } catch (e) {
            }
          } else {
            try {
              fdR = fs.openSync("/dev/tty", "r");
              ttyR = new TTY(fdR, false);
            } catch (e) {
            }
          }
          if (process.stdout.isTTY) {
            fdW = process.stdout.fd;
          } else {
            try {
              fdW = fs.openSync("/dev/tty", "w");
            } catch (e) {
            }
          }
        }
      })();
      (function() {
        var isCooked = !options.hideEchoBack && !options.keyIn, atEol, limit, buffer, reqSize, readSize, chunk, line;
        rawInput = "";
        function setRawMode(mode) {
          if (mode === isRawMode) {
            return true;
          }
          if (ttyR.setRawMode(mode) !== 0) {
            return false;
          }
          isRawMode = mode;
          return true;
        }
        if (_DBG_useExt || !ttyR || typeof fdW !== "number" && (options.display || !isCooked)) {
          input = tryExt();
          return;
        }
        if (options.display) {
          fs.writeSync(fdW, options.display);
          options.display = "";
        }
        if (options.displayOnly) {
          return;
        }
        if (!setRawMode(!isCooked)) {
          input = tryExt();
          return;
        }
        reqSize = options.keyIn ? 1 : options.bufferSize;
        buffer = Buffer.allocUnsafe && Buffer.alloc ? Buffer.alloc(reqSize) : new Buffer(reqSize);
        if (options.keyIn && options.limit) {
          limit = new RegExp(
            "[^" + options.limit + "]",
            "g" + (options.caseSensitive ? "" : "i")
          );
        }
        while (true) {
          readSize = 0;
          try {
            readSize = fs.readSync(fdR, buffer, 0, reqSize);
          } catch (e) {
            if (e.code !== "EOF") {
              setRawMode(false);
              input += tryExt();
              return;
            }
          }
          if (readSize > 0) {
            chunk = buffer.toString(options.encoding, 0, readSize);
            rawInput += chunk;
          } else {
            chunk = "\n";
            rawInput += String.fromCharCode(0);
          }
          if (chunk && typeof (line = (chunk.match(/^(.*?)[\r\n]/) || [])[1]) === "string") {
            chunk = line;
            atEol = true;
          }
          if (chunk) {
            chunk = chunk.replace(/[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]/g, "");
          }
          if (chunk && limit) {
            chunk = chunk.replace(limit, "");
          }
          if (chunk) {
            if (!isCooked) {
              if (!options.hideEchoBack) {
                fs.writeSync(fdW, chunk);
              } else if (options.mask) {
                fs.writeSync(fdW, new Array(chunk.length + 1).join(options.mask));
              }
            }
            input += chunk;
          }
          if (!options.keyIn && atEol || options.keyIn && input.length >= reqSize) {
            break;
          }
        }
        if (!isCooked && !silent) {
          fs.writeSync(fdW, "\n");
        }
        setRawMode(false);
      })();
      if (options.print && !silent) {
        options.print(
          displaySave + (options.displayOnly ? "" : (options.hideEchoBack ? new Array(input.length + 1).join(options.mask) : input) + "\n"),
          options.encoding
        );
      }
      return options.displayOnly ? "" : lastInput = options.keepWhitespace || options.keyIn ? input : input.trim();
    }
    function flattenArray(array, validator) {
      var flatArray = [];
      function _flattenArray(array2) {
        if (array2 == null) {
          return;
        }
        if (Array.isArray(array2)) {
          array2.forEach(_flattenArray);
        } else if (!validator || validator(array2)) {
          flatArray.push(array2);
        }
      }
      _flattenArray(array);
      return flatArray;
    }
    function escapePattern(pattern) {
      return pattern.replace(
        /[\x00-\x7f]/g,
        // eslint-disable-line no-control-regex
        function(s) {
          return "\\x" + ("00" + s.charCodeAt().toString(16)).substr(-2);
        }
      );
    }
    function margeOptions() {
      var optionsList = Array.prototype.slice.call(arguments), optionNames, fromDefault;
      if (optionsList.length && typeof optionsList[0] === "boolean") {
        fromDefault = optionsList.shift();
        if (fromDefault) {
          optionNames = Object.keys(defaultOptions);
          optionsList.unshift(defaultOptions);
        }
      }
      return optionsList.reduce(function(options, optionsPart) {
        if (optionsPart == null) {
          return options;
        }
        if (optionsPart.hasOwnProperty("noEchoBack") && !optionsPart.hasOwnProperty("hideEchoBack")) {
          optionsPart.hideEchoBack = optionsPart.noEchoBack;
          delete optionsPart.noEchoBack;
        }
        if (optionsPart.hasOwnProperty("noTrim") && !optionsPart.hasOwnProperty("keepWhitespace")) {
          optionsPart.keepWhitespace = optionsPart.noTrim;
          delete optionsPart.noTrim;
        }
        if (!fromDefault) {
          optionNames = Object.keys(optionsPart);
        }
        optionNames.forEach(function(optionName) {
          var value;
          if (!optionsPart.hasOwnProperty(optionName)) {
            return;
          }
          value = optionsPart[optionName];
          switch (optionName) {
            //                    _readlineSync <- *    * -> defaultOptions
            // ================ string
            case "mask":
            // *    *
            case "limitMessage":
            //      *
            case "defaultInput":
            //      *
            case "encoding":
              value = value != null ? value + "" : "";
              if (value && optionName !== "limitMessage") {
                value = value.replace(/[\r\n]/g, "");
              }
              options[optionName] = value;
              break;
            // ================ number(int)
            case "bufferSize":
              if (!isNaN(value = parseInt(value, 10)) && typeof value === "number") {
                options[optionName] = value;
              }
              break;
            // ================ boolean
            case "displayOnly":
            // *
            case "keyIn":
            // *
            case "hideEchoBack":
            // *    *
            case "caseSensitive":
            // *    *
            case "keepWhitespace":
            // *    *
            case "history":
            //      *
            case "cd":
              options[optionName] = !!value;
              break;
            // ================ array
            case "limit":
            // *    *     to string for readlineExt
            case "trueValue":
            //      *
            case "falseValue":
              options[optionName] = flattenArray(value, function(value2) {
                var type = typeof value2;
                return type === "string" || type === "number" || type === "function" || value2 instanceof RegExp;
              }).map(function(value2) {
                return typeof value2 === "string" ? value2.replace(/[\r\n]/g, "") : value2;
              });
              break;
            // ================ function
            case "print":
            // *    *
            case "phContent":
            //      *
            case "preCheck":
              options[optionName] = typeof value === "function" ? value : void 0;
              break;
            // ================ other
            case "prompt":
            //      *
            case "display":
              options[optionName] = value != null ? value : "";
              break;
          }
        });
        return options;
      }, {});
    }
    function isMatched(res, comps, caseSensitive) {
      return comps.some(function(comp) {
        var type = typeof comp;
        return type === "string" ? caseSensitive ? res === comp : res.toLowerCase() === comp.toLowerCase() : type === "number" ? parseFloat(res) === comp : type === "function" ? comp(res) : comp instanceof RegExp ? comp.test(res) : false;
      });
    }
    function replaceHomePath(path, expand) {
      var homePath = pathUtil.normalize(
        IS_WIN ? (process.env.HOMEDRIVE || "") + (process.env.HOMEPATH || "") : process.env.HOME || ""
      ).replace(/[/\\]+$/, "");
      path = pathUtil.normalize(path);
      return expand ? path.replace(/^~(?=\/|\\|$)/, homePath) : path.replace(new RegExp("^" + escapePattern(homePath) + "(?=\\/|\\\\|$)", IS_WIN ? "i" : ""), "~");
    }
    function replacePlaceholder(text, generator) {
      var PTN_INNER = "(?:\\(([\\s\\S]*?)\\))?(\\w+|.-.)(?:\\(([\\s\\S]*?)\\))?", rePlaceholder = new RegExp("(\\$)?(\\$<" + PTN_INNER + ">)", "g"), rePlaceholderCompat = new RegExp("(\\$)?(\\$\\{" + PTN_INNER + "\\})", "g");
      function getPlaceholderText(s, escape, placeholder, pre, param, post) {
        var text2;
        return escape || typeof (text2 = generator(param)) !== "string" ? placeholder : text2 ? (pre || "") + text2 + (post || "") : "";
      }
      return text.replace(rePlaceholder, getPlaceholderText).replace(rePlaceholderCompat, getPlaceholderText);
    }
    function array2charlist(array, caseSensitive, collectSymbols) {
      var group = [], groupClass = -1, charCode = 0, symbols = "", values, suppressed;
      function addGroup(groups, group2) {
        if (group2.length > 3) {
          groups.push(group2[0] + "..." + group2[group2.length - 1]);
          suppressed = true;
        } else if (group2.length) {
          groups = groups.concat(group2);
        }
        return groups;
      }
      values = array.reduce(function(chars, value) {
        return chars.concat((value + "").split(""));
      }, []).reduce(function(groups, curChar) {
        var curGroupClass, curCharCode;
        if (!caseSensitive) {
          curChar = curChar.toLowerCase();
        }
        curGroupClass = /^\d$/.test(curChar) ? 1 : /^[A-Z]$/.test(curChar) ? 2 : /^[a-z]$/.test(curChar) ? 3 : 0;
        if (collectSymbols && curGroupClass === 0) {
          symbols += curChar;
        } else {
          curCharCode = curChar.charCodeAt(0);
          if (curGroupClass && curGroupClass === groupClass && curCharCode === charCode + 1) {
            group.push(curChar);
          } else {
            groups = addGroup(groups, group);
            group = [curChar];
            groupClass = curGroupClass;
          }
          charCode = curCharCode;
        }
        return groups;
      }, []);
      values = addGroup(values, group);
      if (symbols) {
        values.push(symbols);
        suppressed = true;
      }
      return { values, suppressed };
    }
    function joinChunks(chunks, suppressed) {
      return chunks.join(chunks.length > 2 ? ", " : suppressed ? " / " : "/");
    }
    function getPhContent(param, options) {
      var resCharlist = {}, text, values, arg;
      if (options.phContent) {
        text = options.phContent(param, options);
      }
      if (typeof text !== "string") {
        switch (param) {
          case "hideEchoBack":
          case "mask":
          case "defaultInput":
          case "caseSensitive":
          case "keepWhitespace":
          case "encoding":
          case "bufferSize":
          case "history":
          case "cd":
            text = !options.hasOwnProperty(param) ? "" : typeof options[param] === "boolean" ? options[param] ? "on" : "off" : options[param] + "";
            break;
          // case 'prompt':
          // case 'query':
          // case 'display':
          //   text = options.hasOwnProperty('displaySrc') ? options.displaySrc + '' : '';
          //   break;
          case "limit":
          case "trueValue":
          case "falseValue":
            values = options[options.hasOwnProperty(param + "Src") ? param + "Src" : param];
            if (options.keyIn) {
              resCharlist = array2charlist(values, options.caseSensitive);
              values = resCharlist.values;
            } else {
              values = values.filter(function(value) {
                var type = typeof value;
                return type === "string" || type === "number";
              });
            }
            text = joinChunks(values, resCharlist.suppressed);
            break;
          case "limitCount":
          case "limitCountNotZero":
            text = options[options.hasOwnProperty("limitSrc") ? "limitSrc" : "limit"].length;
            text = text || param !== "limitCountNotZero" ? text + "" : "";
            break;
          case "lastInput":
            text = lastInput;
            break;
          case "cwd":
          case "CWD":
          case "cwdHome":
            text = process.cwd();
            if (param === "CWD") {
              text = pathUtil.basename(text);
            } else if (param === "cwdHome") {
              text = replaceHomePath(text);
            }
            break;
          case "date":
          case "time":
          case "localeDate":
          case "localeTime":
            text = (/* @__PURE__ */ new Date())["to" + param.replace(/^./, function(str) {
              return str.toUpperCase();
            }) + "String"]();
            break;
          default:
            if (typeof (arg = (param.match(/^history_m(\d+)$/) || [])[1]) === "string") {
              text = inputHistory[inputHistory.length - arg] || "";
            }
        }
      }
      return text;
    }
    function getPhCharlist(param) {
      var matches = /^(.)-(.)$/.exec(param), text = "", from, to, code, step;
      if (!matches) {
        return null;
      }
      from = matches[1].charCodeAt(0);
      to = matches[2].charCodeAt(0);
      step = from < to ? 1 : -1;
      for (code = from; code !== to + step; code += step) {
        text += String.fromCharCode(code);
      }
      return text;
    }
    function parseCl(cl) {
      var reToken = new RegExp(/(\s*)(?:("|')(.*?)(?:\2|$)|(\S+))/g), taken = "", args = [], matches, part;
      cl = cl.trim();
      while (matches = reToken.exec(cl)) {
        part = matches[3] || matches[4] || "";
        if (matches[1]) {
          args.push(taken);
          taken = "";
        }
        taken += part;
      }
      if (taken) {
        args.push(taken);
      }
      return args;
    }
    function toBool(res, options) {
      return options.trueValue.length && isMatched(res, options.trueValue, options.caseSensitive) ? true : options.falseValue.length && isMatched(res, options.falseValue, options.caseSensitive) ? false : res;
    }
    function getValidLine(options) {
      var res, forceNext, limitMessage, matches, histInput, args, resCheck;
      function _getPhContent(param) {
        return getPhContent(param, options);
      }
      function addDisplay(text) {
        options.display += (/[^\r\n]$/.test(options.display) ? "\n" : "") + text;
      }
      options.limitSrc = options.limit;
      options.displaySrc = options.display;
      options.limit = "";
      options.display = replacePlaceholder(options.display + "", _getPhContent);
      while (true) {
        res = _readlineSync(options);
        forceNext = false;
        limitMessage = "";
        if (options.defaultInput && !res) {
          res = options.defaultInput;
        }
        if (options.history) {
          if (matches = /^\s*!(?:!|-1)(:p)?\s*$/.exec(res)) {
            histInput = inputHistory[0] || "";
            if (matches[1]) {
              forceNext = true;
            } else {
              res = histInput;
            }
            addDisplay(histInput + "\n");
            if (!forceNext) {
              options.displayOnly = true;
              _readlineSync(options);
              options.displayOnly = false;
            }
          } else if (res && res !== inputHistory[inputHistory.length - 1]) {
            inputHistory = [res];
          }
        }
        if (!forceNext && options.cd && res) {
          args = parseCl(res);
          switch (args[0].toLowerCase()) {
            case "cd":
              if (args[1]) {
                try {
                  process.chdir(replaceHomePath(args[1], true));
                } catch (e) {
                  addDisplay(e + "");
                }
              }
              forceNext = true;
              break;
            case "pwd":
              addDisplay(process.cwd());
              forceNext = true;
              break;
          }
        }
        if (!forceNext && options.preCheck) {
          resCheck = options.preCheck(res, options);
          res = resCheck.res;
          if (resCheck.forceNext) {
            forceNext = true;
          }
        }
        if (!forceNext) {
          if (!options.limitSrc.length || isMatched(res, options.limitSrc, options.caseSensitive)) {
            break;
          }
          if (options.limitMessage) {
            limitMessage = replacePlaceholder(options.limitMessage, _getPhContent);
          }
        }
        addDisplay((limitMessage ? limitMessage + "\n" : "") + replacePlaceholder(options.displaySrc + "", _getPhContent));
      }
      return toBool(res, options);
    }
    exports2._DBG_set_useExt = function(val) {
      _DBG_useExt = val;
    };
    exports2._DBG_set_checkOptions = function(val) {
      _DBG_checkOptions = val;
    };
    exports2._DBG_set_checkMethod = function(val) {
      _DBG_checkMethod = val;
    };
    exports2._DBG_clearHistory = function() {
      lastInput = "";
      inputHistory = [];
    };
    exports2.setDefaultOptions = function(options) {
      defaultOptions = margeOptions(true, options);
      return margeOptions(true);
    };
    exports2.question = function(query, options) {
      return getValidLine(margeOptions(margeOptions(true, options), {
        display: query
      }));
    };
    exports2.prompt = function(options) {
      var readOptions = margeOptions(true, options);
      readOptions.display = readOptions.prompt;
      return getValidLine(readOptions);
    };
    exports2.keyIn = function(query, options) {
      var readOptions = margeOptions(margeOptions(true, options), {
        display: query,
        keyIn: true,
        keepWhitespace: true
      });
      readOptions.limitSrc = readOptions.limit.filter(function(value) {
        var type = typeof value;
        return type === "string" || type === "number";
      }).map(function(text) {
        return replacePlaceholder(text + "", getPhCharlist);
      });
      readOptions.limit = escapePattern(readOptions.limitSrc.join(""));
      ["trueValue", "falseValue"].forEach(function(optionName) {
        readOptions[optionName] = readOptions[optionName].reduce(function(comps, comp) {
          var type = typeof comp;
          if (type === "string" || type === "number") {
            comps = comps.concat((comp + "").split(""));
          } else {
            comps.push(comp);
          }
          return comps;
        }, []);
      });
      readOptions.display = replacePlaceholder(
        readOptions.display + "",
        function(param) {
          return getPhContent(param, readOptions);
        }
      );
      return toBool(_readlineSync(readOptions), readOptions);
    };
    exports2.questionEMail = function(query, options) {
      if (query == null) {
        query = "Input e-mail address: ";
      }
      return exports2.question(query, margeOptions({
        // -------- default
        hideEchoBack: false,
        // http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
        limit: /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/,
        limitMessage: "Input valid e-mail address, please.",
        trueValue: null,
        falseValue: null
      }, options, {
        // -------- forced
        keepWhitespace: false,
        cd: false
      }));
    };
    exports2.questionNewPassword = function(query, options) {
      var resCharlist, min, max, readOptions = margeOptions({
        // -------- default
        hideEchoBack: true,
        mask: "*",
        limitMessage: "It can include: $<charlist>\nAnd the length must be: $<length>",
        trueValue: null,
        falseValue: null,
        caseSensitive: true
      }, options, {
        // -------- forced
        history: false,
        cd: false,
        // limit (by charlist etc.),
        phContent: function(param) {
          return param === "charlist" ? resCharlist.text : param === "length" ? min + "..." + max : null;
        }
      }), charlist, confirmMessage, unmatchMessage, limit, limitMessage, res1, res2;
      options = options || {};
      charlist = replacePlaceholder(
        options.charlist ? options.charlist + "" : "$<!-~>",
        getPhCharlist
      );
      if (isNaN(min = parseInt(options.min, 10)) || typeof min !== "number") {
        min = 12;
      }
      if (isNaN(max = parseInt(options.max, 10)) || typeof max !== "number") {
        max = 24;
      }
      limit = new RegExp("^[" + escapePattern(charlist) + "]{" + min + "," + max + "}$");
      resCharlist = array2charlist([charlist], readOptions.caseSensitive, true);
      resCharlist.text = joinChunks(resCharlist.values, resCharlist.suppressed);
      confirmMessage = options.confirmMessage != null ? options.confirmMessage : "Reinput a same one to confirm it: ";
      unmatchMessage = options.unmatchMessage != null ? options.unmatchMessage : "It differs from first one. Hit only the Enter key if you want to retry from first one.";
      if (query == null) {
        query = "Input new password: ";
      }
      limitMessage = readOptions.limitMessage;
      while (!res2) {
        readOptions.limit = limit;
        readOptions.limitMessage = limitMessage;
        res1 = exports2.question(query, readOptions);
        readOptions.limit = [res1, ""];
        readOptions.limitMessage = unmatchMessage;
        res2 = exports2.question(confirmMessage, readOptions);
      }
      return res1;
    };
    function _questionNum(query, options, parser) {
      var validValue;
      function getValidValue(value) {
        validValue = parser(value);
        return !isNaN(validValue) && typeof validValue === "number";
      }
      exports2.question(query, margeOptions({
        // -------- default
        limitMessage: "Input valid number, please."
      }, options, {
        // -------- forced
        limit: getValidValue,
        cd: false
        // trueValue, falseValue, caseSensitive, keepWhitespace don't work.
      }));
      return validValue;
    }
    exports2.questionInt = function(query, options) {
      return _questionNum(query, options, function(value) {
        return parseInt(value, 10);
      });
    };
    exports2.questionFloat = function(query, options) {
      return _questionNum(query, options, parseFloat);
    };
    exports2.questionPath = function(query, options) {
      var error = "", validPath, readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        limitMessage: "$<error(\n)>Input valid path, please.$<( Min:)min>$<( Max:)max>",
        history: true,
        cd: true
      }, options, {
        // -------- forced
        keepWhitespace: false,
        limit: function(value) {
          var exists, stat, res;
          value = replaceHomePath(value, true);
          error = "";
          function mkdirParents(dirPath) {
            dirPath.split(/\/|\\/).reduce(function(parents, dir) {
              var path = pathUtil.resolve(parents += dir + pathUtil.sep);
              if (!fs.existsSync(path)) {
                fs.mkdirSync(path);
              } else if (!fs.statSync(path).isDirectory()) {
                throw new Error("Non directory already exists: " + path);
              }
              return parents;
            }, "");
          }
          try {
            exists = fs.existsSync(value);
            validPath = exists ? fs.realpathSync(value) : pathUtil.resolve(value);
            if (!options.hasOwnProperty("exists") && !exists || typeof options.exists === "boolean" && options.exists !== exists) {
              error = (exists ? "Already exists" : "No such file or directory") + ": " + validPath;
              return false;
            }
            if (!exists && options.create) {
              if (options.isDirectory) {
                mkdirParents(validPath);
              } else {
                mkdirParents(pathUtil.dirname(validPath));
                fs.closeSync(fs.openSync(validPath, "w"));
              }
              validPath = fs.realpathSync(validPath);
            }
            if (exists && (options.min || options.max || options.isFile || options.isDirectory)) {
              stat = fs.statSync(validPath);
              if (options.isFile && !stat.isFile()) {
                error = "Not file: " + validPath;
                return false;
              } else if (options.isDirectory && !stat.isDirectory()) {
                error = "Not directory: " + validPath;
                return false;
              } else if (options.min && stat.size < +options.min || options.max && stat.size > +options.max) {
                error = "Size " + stat.size + " is out of range: " + validPath;
                return false;
              }
            }
            if (typeof options.validate === "function" && (res = options.validate(validPath)) !== true) {
              if (typeof res === "string") {
                error = res;
              }
              return false;
            }
          } catch (e) {
            error = e + "";
            return false;
          }
          return true;
        },
        // trueValue, falseValue, caseSensitive don't work.
        phContent: function(param) {
          return param === "error" ? error : param !== "min" && param !== "max" ? null : options.hasOwnProperty(param) ? options[param] + "" : "";
        }
      });
      options = options || {};
      if (query == null) {
        query = 'Input path (you can "cd" and "pwd"): ';
      }
      exports2.question(query, readOptions);
      return validPath;
    };
    function getClHandler(commandHandler, options) {
      var clHandler = {}, hIndex = {};
      if (typeof commandHandler === "object") {
        Object.keys(commandHandler).forEach(function(cmd) {
          if (typeof commandHandler[cmd] === "function") {
            hIndex[options.caseSensitive ? cmd : cmd.toLowerCase()] = commandHandler[cmd];
          }
        });
        clHandler.preCheck = function(res) {
          var cmdKey;
          clHandler.args = parseCl(res);
          cmdKey = clHandler.args[0] || "";
          if (!options.caseSensitive) {
            cmdKey = cmdKey.toLowerCase();
          }
          clHandler.hRes = cmdKey !== "_" && hIndex.hasOwnProperty(cmdKey) ? hIndex[cmdKey].apply(res, clHandler.args.slice(1)) : hIndex.hasOwnProperty("_") ? hIndex._.apply(res, clHandler.args) : null;
          return { res, forceNext: false };
        };
        if (!hIndex.hasOwnProperty("_")) {
          clHandler.limit = function() {
            var cmdKey = clHandler.args[0] || "";
            if (!options.caseSensitive) {
              cmdKey = cmdKey.toLowerCase();
            }
            return hIndex.hasOwnProperty(cmdKey);
          };
        }
      } else {
        clHandler.preCheck = function(res) {
          clHandler.args = parseCl(res);
          clHandler.hRes = typeof commandHandler === "function" ? commandHandler.apply(res, clHandler.args) : true;
          return { res, forceNext: false };
        };
      }
      return clHandler;
    }
    exports2.promptCL = function(commandHandler, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        limitMessage: "Requested command is not available.",
        caseSensitive: false,
        history: true
      }, options), clHandler = getClHandler(commandHandler, readOptions);
      readOptions.limit = clHandler.limit;
      readOptions.preCheck = clHandler.preCheck;
      exports2.prompt(readOptions);
      return clHandler.args;
    };
    exports2.promptLoop = function(inputHandler, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        trueValue: null,
        falseValue: null,
        caseSensitive: false,
        history: true
      }, options);
      while (true) {
        if (inputHandler(exports2.prompt(readOptions))) {
          break;
        }
      }
    };
    exports2.promptCLLoop = function(commandHandler, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        limitMessage: "Requested command is not available.",
        caseSensitive: false,
        history: true
      }, options), clHandler = getClHandler(commandHandler, readOptions);
      readOptions.limit = clHandler.limit;
      readOptions.preCheck = clHandler.preCheck;
      while (true) {
        exports2.prompt(readOptions);
        if (clHandler.hRes) {
          break;
        }
      }
    };
    exports2.promptSimShell = function(options) {
      return exports2.prompt(margeOptions({
        // -------- default
        hideEchoBack: false,
        history: true
      }, options, {
        // -------- forced
        prompt: function() {
          return IS_WIN ? "$<cwd>>" : (
            // 'user@host:cwd$ '
            (process.env.USER || "") + (process.env.HOSTNAME ? "@" + process.env.HOSTNAME.replace(/\..*$/, "") : "") + ":$<cwdHome>$ "
          );
        }()
      }));
    };
    function _keyInYN(query, options, limit) {
      var res;
      if (query == null) {
        query = "Are you sure? ";
      }
      if ((!options || options.guide !== false) && (query += "")) {
        query = query.replace(/\s*:?\s*$/, "") + " [y/n]: ";
      }
      res = exports2.keyIn(query, margeOptions(options, {
        // -------- forced
        hideEchoBack: false,
        limit,
        trueValue: "y",
        falseValue: "n",
        caseSensitive: false
        // mask doesn't work.
      }));
      return typeof res === "boolean" ? res : "";
    }
    exports2.keyInYN = function(query, options) {
      return _keyInYN(query, options);
    };
    exports2.keyInYNStrict = function(query, options) {
      return _keyInYN(query, options, "yn");
    };
    exports2.keyInPause = function(query, options) {
      if (query == null) {
        query = "Continue...";
      }
      if ((!options || options.guide !== false) && (query += "")) {
        query = query.replace(/\s+$/, "") + " (Hit any key)";
      }
      exports2.keyIn(query, margeOptions({
        // -------- default
        limit: null
      }, options, {
        // -------- forced
        hideEchoBack: true,
        mask: ""
      }));
    };
    exports2.keyInSelect = function(items, query, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false
      }, options, {
        // -------- forced
        trueValue: null,
        falseValue: null,
        caseSensitive: false,
        // limit (by items),
        phContent: function(param) {
          return param === "itemsCount" ? items.length + "" : param === "firstItem" ? (items[0] + "").trim() : param === "lastItem" ? (items[items.length - 1] + "").trim() : null;
        }
      }), keylist = "", key2i = {}, charCode = 49, display = "\n";
      if (!Array.isArray(items) || !items.length || items.length > 35) {
        throw "`items` must be Array (max length: 35).";
      }
      items.forEach(function(item, i) {
        var key = String.fromCharCode(charCode);
        keylist += key;
        key2i[key] = i;
        display += "[" + key + "] " + (item + "").trim() + "\n";
        charCode = charCode === 57 ? 97 : charCode + 1;
      });
      if (!options || options.cancel !== false) {
        keylist += "0";
        key2i["0"] = -1;
        display += "[0] " + (options && options.cancel != null && typeof options.cancel !== "boolean" ? (options.cancel + "").trim() : "CANCEL") + "\n";
      }
      readOptions.limit = keylist;
      display += "\n";
      if (query == null) {
        query = "Choose one from list: ";
      }
      if (query += "") {
        if (!options || options.guide !== false) {
          query = query.replace(/\s*:?\s*$/, "") + " [$<limit>]: ";
        }
        display += query;
      }
      return key2i[exports2.keyIn(display, readOptions).toLowerCase()];
    };
    exports2.getRawInput = function() {
      return rawInput;
    };
    function _setOption(optionName, args) {
      var options;
      if (args.length) {
        options = {};
        options[optionName] = args[0];
      }
      return exports2.setDefaultOptions(options)[optionName];
    }
    exports2.setPrint = function() {
      return _setOption("print", arguments);
    };
    exports2.setPrompt = function() {
      return _setOption("prompt", arguments);
    };
    exports2.setEncoding = function() {
      return _setOption("encoding", arguments);
    };
    exports2.setMask = function() {
      return _setOption("mask", arguments);
    };
    exports2.setBufferSize = function() {
      return _setOption("bufferSize", arguments);
    };
  }
});

// rts.js
var util_ = require("util");
var reader_ = require_readline_sync();
var debug_ = (x) => {
  console.log(util_.inspect(x, false, null));
};
var _Var = "Var";
var _Let = "Let";
var _Lam = "Lam";
var _App = "App";
var _Quote = "Quote";
var _Splice = "Splice";
var _Return = "Return";
var _Bind = "Bind";
var _Seq = "Seq";
var _New = "New";
var _Write = "Write";
var _Read = "Read";
var _Closure = "Closure";
var _CSP = "CSP";
var _LiftedLam = "LiftedLam";
var _Body = "Body";
var _NatElim = "NatElim";
var _ReadNat = "ReadNat";
var _PrintNat = "PrintNat";
var _Log = "Log";
var _Rec = "Rec";
var _Suc = "Suc";
var _Proj = "Proj";
var _NatLit = "NatLit";
function Var_(x) {
  return { tag: _Var, name: x };
}
function Let_(x, t, u) {
  return { tag: _Let, _1: x, _2: t, _3: u };
}
function Lam_(x, t) {
  return { tag: _Lam, _1: x, _2: t };
}
function App_(t, u) {
  return { tag: _App, _1: t, _2: u };
}
function Quote_(t) {
  return { tag: _Quote, _1: t };
}
function Splice_(t) {
  return { tag: _Splice, _1: t };
}
function Return_(t) {
  return { tag: _Return, _1: t };
}
function Bind_(x, t, u) {
  return { tag: _Bind, _1: x, _2: t, _3: u };
}
function Seq_(t, u) {
  return { tag: _Seq, _1: t, _2: u };
}
function New_(t) {
  return { tag: _New, _1: t };
}
function Write_(t, u) {
  return { tag: _Write, _1: t, _2: u };
}
function Read_(t) {
  return { tag: _Read, _1: t };
}
function CSP_(t, x) {
  return { tag: _CSP, _1: t, _2: x };
}
function NatElim_(s, z, n) {
  return { tag: _NatElim, _1: s, _2: z, _3: n };
}
function ReadNat_() {
  return { tag: _ReadNat };
}
function PrintNat_(t) {
  return { tag: _PrintNat, _1: t };
}
function Log_(s) {
  return { tag: _Log, _1: s };
}
function Rec_(ts) {
  return { tag: _Rec, _1: ts };
}
function Suc_(n) {
  return { tag: _Suc, _1: n };
}
function Proj_(t, x) {
  return { tag: _Proj, _1: t, _2: x };
}
var CSP_undefined_ = CSP_(void 0, "undefined");
function TVar_(x) {
  return { tag: _Var, _1: x };
}
function TCSP_(i, x) {
  return { tag: _CSP, _1: i, _2: x };
}
function TLet_(x, t, u) {
  return { tag: _Let, _1: x, _2: t, _3: u };
}
function TLam_(x, t) {
  return { tag: _Lam, _1: x, _2: t };
}
function TLiftedLam_(f, x, args) {
  return { tag: _LiftedLam, _1: f, _2: x, _3: args };
}
function TApp_(t, u) {
  return { tag: _App, _1: t, _2: u };
}
function TQuote_(t) {
  return { tag: _Quote, _1: t };
}
function TSplice_(t) {
  return { tag: _Splice, _1: t };
}
function TReturn_(t) {
  return { tag: _Return, _1: t };
}
function TBind_(x, t, u) {
  return { tag: _Bind, _1: x, _2: t, _3: u };
}
function TSeq_(t, u) {
  return { tag: _Seq, _1: t, _2: u };
}
function TNew_(t) {
  return { tag: _New, _1: t };
}
function TWrite_(t, u) {
  return { tag: _Write, _1: t, _2: u };
}
function TRead_(t) {
  return { tag: _Read, _1: t };
}
function TNatLit_(t) {
  return { tag: _NatLit, _1: t };
}
function TReadNat_() {
  return { tag: _ReadNat };
}
function TPrintNat_(t) {
  return { tag: _PrintNat, _1: t };
}
function TRec_(ts) {
  return { tag: _Rec, _1: ts };
}
function TProj_(t, x) {
  return { tag: _Proj, _1: t, _2: x };
}
function TSuc_(t) {
  return { tag: _Suc, _1: t };
}
function TNatElim_(s, z, n) {
  return { tag: _NatElim, _1: s, _2: z, _3: n };
}
function TLog_(x) {
  return { tag: _Log, _1: x };
}
function TopLet_(x, t, u) {
  return { tag: _Let, _1: x, _2: t, _3: u };
}
function TopBind_(x, t, u) {
  return { tag: _Bind, _1: x, _2: t, _3: u };
}
function TopSeq_(t, u) {
  return { tag: _Seq, _1: t, _2: u };
}
function TopBody_(t) {
  return { tag: _Body, _1: t };
}
function TopClosure_(x, env, arg, body, t) {
  return { tag: _Closure, _1: x, _2: env, _3: arg, _4: body, _5: t };
}
var boundVarSet_ = /* @__PURE__ */ new Set();
function freshenName_(x) {
  let res = x;
  while (boundVarSet_.has(res)) {
    res = res + boundVarSet_.size;
  }
  return res;
}
function displayCode_(loc, code) {
  if (loc) {
    console.log("CODE GENERATED AT:");
    for (const l of loc) {
      console.log(l);
    }
  } else {
    console.log("CODE GENERATED:");
  }
  console.log("CODE:");
  console.log(code);
  console.log("");
}
function evalCodeGenClosed_(src_, csp_, loc_) {
  displayCode_(loc_, src_);
  const res_ = eval(src_)();
  return res_;
}
function codegenClosed_(t, loc) {
  function closureConvert(top) {
    let stage = void 0;
    const inStage = (s, act) => {
      const backup = stage;
      stage = s;
      const res2 = act();
      stage = backup;
      return res2;
    };
    let freeVars = /* @__PURE__ */ new Set();
    let topVars = /* @__PURE__ */ new Set();
    let closures = new Array();
    let cspArray2 = new Array();
    let currentTopName = "";
    function goTop(top2) {
      function addClosures(cs, t3) {
        let res2 = t3;
        for (const cl of cs.reverse()) {
          res2 = TopClosure_(cl.name, cl.env, cl.arg, cl.body, res2);
        }
        return res2;
      }
      function resetAtTop(name) {
        currentTopName = name;
        freeVars.clear();
        closures = new Array();
      }
      switch (top2.tag) {
        case _Let: {
          const x = freshenName_(top2._1);
          const t3 = top2._2;
          const u = top2._3;
          resetAtTop(x);
          const t22 = go(t3);
          const newClosures = closures;
          boundVarSet_.add(x);
          topVars.add(x);
          const u2 = goTop(u(Var_(x)));
          return addClosures(newClosures, TopLet_(x, t22, u2));
        }
        case _Bind: {
          const x = freshenName_(top2._1);
          const t3 = top2._2;
          const u = top2._3;
          resetAtTop(x);
          const t22 = go(t3);
          const newClosures = closures;
          boundVarSet_.add(x);
          topVars.add(x);
          const u2 = goTop(u(Var_(x)));
          return addClosures(newClosures, TopBind_(x, t22, u2));
        }
        case _Seq: {
          const t3 = top2._1;
          const u = top2._2;
          resetAtTop("$cl");
          const t22 = go(t3);
          const newClosures = closures;
          const u2 = goTop(u);
          return addClosures(newClosures, TopSeq_(t22, u2));
        }
        default: {
          resetAtTop("$cl");
          const t22 = go(top2);
          return addClosures(closures, TopBody_(t22));
        }
      }
    }
    function fresh(x, act) {
      const x2 = freshenName_(x);
      boundVarSet_.add(x2);
      const res2 = act(x2);
      boundVarSet_.delete(x);
      return res2;
    }
    const bind = (x, act) => fresh(x, (x2) => {
      const a = act(x2);
      freeVars.delete(x2);
      return a;
    });
    function go(top2) {
      switch (top2.tag) {
        case _Var: {
          if (!topVars.has(top2.name)) {
            freeVars.add(top2.name);
          }
          return TVar_(top2.name);
        }
        case _Let: {
          const x = top2._1;
          const t3 = top2._2;
          const u = top2._3;
          const t22 = go(t3);
          return bind(x, (x2) => TLet_(x2, t22, go(u(Var_(x2)))));
        }
        case _Lam: {
          const x = top2._1;
          const t3 = top2._2;
          if (stage === void 0) {
            return fresh(x, (x2) => {
              let oldFreeVars = freeVars;
              freeVars = /* @__PURE__ */ new Set();
              const t22 = go(t3(Var_(x2)));
              freeVars.delete(x2);
              const capture = Array.from(freeVars.values());
              const clName = currentTopName + closures.length + "_";
              closures.push({ name: clName, env: capture, arg: x2, body: t22 });
              freeVars.forEach((x3) => oldFreeVars.add(x3));
              freeVars = oldFreeVars;
              return TLiftedLam_(clName, x2, capture);
            });
          } else {
            return bind(x, (x2) => TLam_(x2, go(t3(Var_(x2)))));
          }
        }
        case _App:
          return TApp_(go(top2._1), go(top2._2));
        case _Quote: {
          if (stage === void 0) {
            return inStage(1, () => TQuote_(go(top2._1)));
          } else {
            return inStage(stage + 1, () => TQuote_(go(top2._1)));
          }
        }
        case _Splice: {
          if (stage && stage > 0) {
            return inStage(stage - 1, () => TSplice_(go(top2._1)));
          } else {
            return TSplice_(go(top2._1));
          }
        }
        case _Bind: {
          const t22 = go(top2._2);
          return bind(top2._1, (x) => TBind_(x, t22, go(top2._3(Var_(x)))));
        }
        case _Return:
          return TReturn_(go(top2._1));
        case _Seq:
          return TSeq_(go(top2._1), go(top2._2));
        case _New:
          return TNew_(go(top2._1));
        case _Write:
          return TWrite_(go(top2._1), go(top2._2));
        case _Read:
          return TRead_(go(top2._1));
        case _CSP: {
          if (typeof top2._1 === "number") {
            return TNatLit_(top2._1);
          } else {
            const id = cspArray2.length;
            cspArray2.push(top2._1);
            return TCSP_(id, top2._2);
          }
        }
        case _Suc:
          return TSuc_(go(top2._1));
        case _NatElim:
          return TNatElim_(go(top2._1), go(top2._2), go(top2._3));
        case _Rec: {
          const res2 = /* @__PURE__ */ new Map();
          top2._1.forEach((t3, x) => {
            res2.set(x, go(t3));
          });
          return TRec_(res2);
        }
        case _Proj:
          return TProj_(go(top2._1), top2._2);
        case _PrintNat:
          return TPrintNat_(go(top2._1));
        case _ReadNat:
          return TReadNat_();
        case _Log:
          return TLog_(top2._1);
      }
    }
    const res = goTop(top);
    return { _1: res, _2: cspArray2 };
  }
  function emitCode(t3) {
    const builder = new Array();
    const build = () => {
      return builder.join("");
    };
    let indentation = 0;
    let isTail = true;
    let stage = 0;
    const closedVars = /* @__PURE__ */ new Set();
    const put = (s) => {
      builder.push(s);
    };
    const str = (s) => () => put(s);
    const strLit = (s) => () => put("`" + s + "`");
    const newl = () => {
      put("\n" + " ".repeat(indentation));
    };
    const inStage = (s, act) => () => {
      const backup = stage;
      stage = s;
      const res = act();
      stage = backup;
      return res;
    };
    const tail = (act) => () => {
      const backup = isTail;
      isTail = true;
      const res = act();
      isTail = backup;
      return res;
    };
    const nonTail = (act) => () => {
      const backup = isTail;
      isTail = false;
      const res = act();
      isTail = backup;
      return res;
    };
    const indent = (act) => () => {
      const backup = indentation;
      indentation += 2;
      const res = act();
      indentation = backup;
      return res;
    };
    function semi() {
      put(";");
    }
    const par = (act) => () => {
      put("(");
      act();
      put(")");
    };
    const bind = (x, closed2, act) => () => {
      if (closed2) {
        closedVars.add(x);
      }
      ;
      const res = act();
      if (closed2) {
        closedVars.delete(x);
      }
      return res;
    };
    const jLet = (x, closed2, t4, u) => () => {
      if (isTail) {
        put("const " + x + " = ");
        indent(nonTail(t4))();
        semi();
        newl();
        tail(bind(x, closed2, u))();
      } else {
        put("((" + x + ") => ");
        par(nonTail(bind(x, closed2, u)))();
        put(")(");
        nonTail(t4)();
        put(")");
      }
    };
    const jSeq = (t4, u) => () => {
      if (isTail) {
        indent(nonTail(t4))();
        semi();
        newl();
        tail(u)();
      } else {
        put("((_) => ");
        par(nonTail(u))();
        put(")(");
        nonTail(t4)();
        put(")");
      }
    };
    const jTuple = (xs) => () => {
      if (xs.length === 0) {
        put("()");
      } else {
        put("(");
        xs[0]();
        xs.slice(1, xs.length).forEach((act) => {
          put(", ");
          act();
        });
        put(")");
      }
    };
    const jReturn = (t4) => () => {
      if (isTail) {
        put("return ");
        nonTail(t4)();
      } else {
        t4();
      }
    };
    const jLam = (xs, closed2, t4) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(" => {");
        tail(() => {
          if (closed2) {
            xs.forEach((x) => closedVars.add(x));
          }
          t4();
          if (closed2) {
            xs.forEach((x) => closedVars.delete(x));
          }
        })();
        put("}");
      })();
    };
    const jLamExp = (xs, closed2, t4) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(" => ");
        nonTail(() => {
          if (closed2) {
            xs.forEach((x) => closedVars.add(x));
          }
          ;
          t4();
          if (closed2) {
            xs.forEach((x) => closedVars.delete(x));
          }
          ;
        })();
      })();
    };
    const cApp = (t4, u) => () => {
      jReturn(() => {
        par(t4)();
        put("._1");
        par(u)();
      })();
    };
    const cRec = (ts) => () => {
      const arr = Array.from(ts);
      function go(i) {
        if (i == arr.length) {
          return;
        } else if (arr.length !== 0 && i == arr.length - 1) {
          put(arr[i][0]);
          put(": ");
          return nonTail(() => ceval(arr[i][1]))();
        } else {
          put(arr[i][0]);
          put(": ");
          nonTail(() => ceval(arr[i][1]))();
          put(", ");
          return go(i + 1);
        }
      }
      go(0);
    };
    const jApp = (t4, args) => () => {
      jReturn(() => {
        t4();
        jTuple(args)();
      })();
    };
    const cRun = (t4) => () => {
      jReturn(() => {
        t4();
        put("()");
      })();
    };
    const jClosure = (env, x, closed2, t4) => () => {
      if (env.length === 0) {
        jLam([x], closed2, t4)();
      } else {
        jLamExp(env, closed2, jLam([x], closed2, t4))();
      }
    };
    const jAppClosure = (t4, args) => () => {
      if (args.length === 0) {
        t4();
      } else {
        t4();
        jTuple(args)();
      }
    };
    const oRec = (ts) => () => {
      const arr = Array.from(ts);
      function go(i) {
        if (i == arr.length) {
          return;
        } else if (arr.length !== 0 && i == arr.length - 1) {
          put("[");
          strLit(arr[i][0])();
          put(", ");
          nonTail(() => oeval(arr[i][1]))();
          put("]");
        } else {
          put("[");
          strLit(arr[i][0])();
          put(", ");
          nonTail(() => oeval(arr[i][1]))();
          put("], ");
          return go(i + 1);
        }
      }
      go(0);
    };
    const closeVar = (x) => x + "c";
    const openVar = (x) => x + "o";
    function exec(top) {
      switch (top.tag) {
        case _Var:
          return cRun(() => put(top._1))();
        case _Let:
          return jLet(top._1, true, () => ceval(top._2), () => exec(top._3))();
        case _Lam:
          throw new Error("impossible");
        case _LiftedLam:
          throw new Error("impossible");
        case _App:
          return cRun(cApp(() => ceval(top._1), () => ceval(top._2)))();
        case _Quote:
          throw new Error("impossible");
        case _Splice:
          return jApp(str("codegen"), [() => ceval(top._1)])();
        case _Return:
          return jReturn(() => ceval(top._1))();
        case _Bind:
          return jLet(top._1, true, () => exec(top._2), () => exec(top._3))();
        case _Seq:
          return jSeq(() => exec(top._1), () => exec(top._2))();
        case _New:
          return jReturn(() => {
            put("{_1 : ");
            ceval(top._1);
            put("}");
          })();
        case _Write:
          return nonTail(() => {
            ceval(top._1);
            put("._1 = ");
            ceval(top._2);
            jReturn(str("{}"))();
          })();
        case _Read:
          return jReturn(() => {
            ceval(top._1);
            put("._1");
          })();
        case _CSP:
          return jReturn(() => {
            put("csp_[" + top._1 + "]()/*");
            strLit(top._2);
            put("*/");
          })();
        case _Log:
          return jApp(str("log_"), [])();
        case _ReadNat:
          return jApp(str("readNat_"), [])();
        case _PrintNat:
          return jApp(str("printNat_"), [() => ceval(top._1)])();
        case _Rec:
          throw new Error("impossible");
        case _Proj:
          return cRun(() => {
            ceval(top._1);
            put(".");
            put(top._2);
          })();
        case _Suc:
          throw new Error("impossible");
        case _NatElim:
          return cRun(jApp(str("cNatElim_"), [() => ceval(top._1), () => ceval(top._2), () => ceval(top._3)]))();
        case _NatLit:
          throw new Error("impossible");
      }
    }
    function ceval(top) {
      switch (top.tag) {
        case _Var:
          return jReturn(str(top._1))();
        case _Let:
          return jLet(top._1, true, () => ceval(top._2), () => ceval(top._3))();
        case _Lam:
          throw new Error("impossible");
        case _LiftedLam:
          return jReturn(() => {
            put("{_1 : ");
            jAppClosure(str(closeVar(top._1)), top._3.map(str))();
            put(", _2 : ");
            jAppClosure(str(openVar(top._1)), top._3.map(oevalVar))();
            put("}");
          })();
        case _App:
          return cApp(() => ceval(top._1), () => ceval(top._2))();
        case _Quote:
          return inStage(1, () => {
            return oeval(top._1);
          })();
        case _Splice:
          return jApp(str("codegenClosed_"), [() => ceval(top._1)])();
        case _Return:
          return jLam([], true, () => exec(top))();
        case _Bind:
          return jLam([], true, () => exec(top))();
        case _Seq:
          return jLam([], true, () => exec(top))();
        case _New:
          return jLam([], true, () => exec(top))();
        case _Write:
          return jLam([], true, () => exec(top))();
        case _Read:
          return jLam([], true, () => exec(top))();
        case _CSP:
          return jReturn(() => {
            put("csp_[" + top._1 + "]/*");
            put(top._2);
            put("*/");
          })();
        case _Log:
          return jLam([], true, () => exec(top))();
        case _ReadNat:
          return jLam([], true, () => exec(top))();
        case _PrintNat:
          return jLam([], true, () => exec(top))();
        case _Rec:
          return jReturn(() => {
            put("{");
            cRec(top._1)();
            put("}");
          })();
        case _Proj:
          return jReturn(() => {
            ceval(top._1);
            put(".");
            put(top._2);
          })();
        case _Suc:
          return jApp(str("cSuc_"), [() => ceval(top._1)])();
        case _NatElim:
          return jApp(str("cNatElim_"), [() => ceval(top._1), () => ceval(top._2), () => ceval(top._3)])();
        case _NatLit:
          return jReturn(str(top._1.toString()))();
      }
    }
    const oevalVar = (x) => () => {
      if (closedVars.has(x)) {
        return jApp(str("CSP_"), [str(x), strLit(x)])();
      } else {
        return jReturn(str(x))();
      }
    };
    function oeval(top) {
      switch (top.tag) {
        case _Var:
          return oevalVar(top._1)();
        case _CSP:
          return jApp(str("CSP_"), [str("csp_[" + top._1 + "]"), strLit(top._2)])();
        case _Let: {
          if (stage === 0) {
            return jLet(top._1, false, () => oeval(top._2), () => oeval(top._3))();
          } else {
            return jApp(str("Let_"), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])();
          }
        }
        case _Lam: {
          return jApp(str("Lam_"), [strLit(top._1), jLam([top._1], false, () => oeval(top._2))])();
        }
        case _LiftedLam:
          return jApp(str("Lam_"), [strLit(top._2), jAppClosure(str(openVar(top._1)), top._3.map(str))])();
        case _App: {
          if (stage === 0) {
            return jApp(str("app_"), [() => oeval(top._1), () => oeval(top._2)])();
          } else {
            return jApp(str("App_"), [() => oeval(top._1), () => oeval(top._2)])();
          }
        }
        case _Quote:
          return jApp(str("quote_"), [inStage(stage + 1, () => oeval(top._1))])();
        case _Splice: {
          if (stage === 0) {
            return jApp(str("codegenOpen_"), [() => oeval(top._1)])();
          } else {
            return inStage(stage - 1, jApp(str("splice_"), [() => oeval(top._1)]))();
          }
        }
        case _Return:
          return jApp(str("Return_"), [() => oeval(top._1)])();
        case _Bind:
          return jApp(str("Bind_"), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])();
        case _Seq:
          return jApp(str("Seq_"), [() => oeval(top._1), () => oeval(top._2)])();
        case _New:
          return jApp(str("New_"), [() => oeval(top._1)])();
        case _Write:
          return jApp(str("Write_"), [() => oeval(top._1), () => oeval(top._2)])();
        case _Read:
          return jApp(str("Read_"), [() => oeval(top._1)])();
        case _Log:
          return jApp(str("Log_"), [strLit(top._1)])();
        case _ReadNat:
          return jApp(str("ReadNat_"), [])();
        case _PrintNat:
          return jApp(str("PrintNat_"), [() => oeval(top._1)])();
        case _Rec:
          return jApp(str("Rec_"), [() => {
            put("new Map([");
            oRec(top._1)();
            put("])");
          }])();
        case _Proj: {
          if (stage === 0) {
            return jApp(str("proj_"), [() => oeval(top._1), strLit(top._2)])();
          } else {
            return jApp(str("Proj_"), [() => oeval(top._1), strLit(top._2)])();
          }
        }
        case _Suc: {
          if (stage === 0) {
            return jApp(str("suc_"), [() => oeval(top._1)])();
          } else {
            return jApp(str("Suc_"), [() => oeval(top._1)])();
          }
        }
        case _NatElim: {
          if (stage === 0) {
            return jApp(str("natElim_"), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])();
          } else {
            return jApp(str("NatElim_"), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])();
          }
        }
        case _NatLit: {
          return jApp(str("CSP_"), [str(top._1.toString()), strLit("")])();
        }
      }
    }
    function execTop(top) {
      tail(() => {
        switch (top.tag) {
          case _Let:
            return jLet(top._1, true, () => ceval(top._2), () => {
              execTop(top._3);
            })();
          case _Bind:
            return jLet(top._1, true, () => exec(top._2), () => {
              execTop(top._3);
            })();
          case _Seq:
            return jSeq(() => exec(top._1), () => {
              execTop(top._2);
            })();
          case _Closure: {
            const x = top._1;
            const env = top._2;
            const arg = top._3;
            const body = top._4;
            const t4 = top._5;
            return jLet(
              closeVar(x),
              true,
              jClosure(env, arg, true, () => ceval(body)),
              jLet(openVar(x), true, jClosure(env, arg, false, inStage(0, () => {
                return oeval(body);
              })), () => execTop(t4))
            )();
          }
          case _Body: {
            return exec(top._1);
          }
        }
      })();
    }
    function cevalTop(top) {
      switch (top.tag) {
        case _Let: {
          return jLet(top._1, true, () => ceval(top._2), () => {
            newl();
            cevalTop(top._3);
          })();
        }
        case _Bind: {
          return jLam([], true, () => execTop(top))();
        }
        case _Seq: {
          return jLam([], true, () => execTop(top))();
        }
        case _Closure: {
          const x = top._1;
          const env = top._2;
          const arg = top._3;
          const body = top._4;
          const t4 = top._5;
          return jLet(
            closeVar(x),
            true,
            jClosure(env, arg, true, () => ceval(body)),
            jLet(openVar(x), true, jClosure(env, arg, false, inStage(0, () => {
              return oeval(body);
            })), () => cevalTop(t4))
          )();
        }
        case _Body:
          return ceval(top._1);
      }
    }
    function oevalTop(top) {
      switch (top.tag) {
        case _Let: {
          if (stage === 0) {
            return jLet(top._1, false, () => oeval(top._2), () => {
              newl();
              oevalTop(top._3);
            })();
          } else {
            return jApp(str("Let_"), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oevalTop(top._3))])();
          }
        }
        case _Bind:
          return jApp(str("Bind_"), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oevalTop(top._3))])();
        case _Seq:
          return jApp(str("Seq_"), [() => oeval(top._1), () => oevalTop(top._2)])();
        case _Closure:
          throw new Error("impossible");
        case _Body:
          return oeval(top._1);
      }
    }
    put("() => {\n");
    cevalTop(t3);
    put(";\n}");
    return build();
  }
  const { _1: t2, _2: cspArray } = closureConvert(t);
  const source = emitCode(t2);
  return evalCodeGenClosed_(source, cspArray, loc);
}
function evalCodeGenOpen_(src_, csp_, loc_) {
  displayCode_(loc_, src_);
  const res_ = eval(src_)();
  return res_;
}
function codegenOpen_(t, loc) {
  function quote(top) {
    const cspArray = new Array();
    function bind(x, act) {
      const x2 = freshenName_(x);
      boundVarSet_.add(x2);
      const res = act(x2);
      boundVarSet_.delete(x);
      return res;
    }
    function go(top2) {
      switch (top2.tag) {
        case _Var:
          return TVar_(top2.name);
        case _Let: {
          const x = top2._1;
          const t2 = top2._2;
          const u = top2._3;
          const t22 = go(t2);
          return bind(x, (x2) => TLet_(x2, t22, go(u(Var_(x2)))));
        }
        case _Lam:
          return bind(top2._1, (x) => TLam_(x, go(top2._2(Var_(x)))));
        case _App:
          return TApp_(go(top2._1), go(top2._2));
        case _Quote:
          return TQuote_(go(top2._1));
        case _Splice:
          return TSplice_(go(top2._1));
        case _Bind: {
          const t2 = go(top2._2);
          return bind(top2._1, (x) => TBind_(x, t2, go(top2._3(Var_(x)))));
        }
        case _Return:
          return TReturn_(go(top2._1));
        case _Seq:
          return TSeq_(go(top2._1), go(top2._2));
        case _New:
          return TNew_(go(top2._1));
        case _Write:
          return TWrite_(go(top2._1), go(top2._2));
        case _Read:
          return TRead_(go(top2._1));
        case _CSP: {
          if (typeof top2._1 === "number") {
            return TNatLit_(top2._1);
          } else {
            const id = cspArray.length;
            cspArray.push(top2._1);
            return TCSP_(id, top2._2);
          }
        }
        case _Suc:
          return TSuc_(go(top2._1));
        case _NatElim:
          return TNatElim_(go(top2._1), go(top2._2), go(top2._3));
        case _Rec: {
          const res = /* @__PURE__ */ new Map();
          top2._1.forEach((t2, x) => {
            res.set(x, go(t2));
          });
          return TRec_(res);
        }
        case _Proj:
          return TProj_(go(top2._1), top2._2);
        case _PrintNat:
          return TPrintNat_(go(top2._1));
        case _ReadNat:
          return TReadNat_();
        case _Log:
          return TLog_(top2._1);
      }
    }
    return { _1: go(top), _2: cspArray };
  }
  function emitCode(t2) {
    const builder = new Array();
    const build = () => {
      return builder.join("");
    };
    const localVars = /* @__PURE__ */ new Set();
    let indentation = 0;
    let isTail = true;
    let stage = 0;
    const put = (s) => {
      builder.push(s);
    };
    const str = (s) => () => put(s);
    const strLit = (s) => () => put("`" + s + "`");
    const newl = () => {
      put("\n" + " ".repeat(indentation));
    };
    const inStage = (s, act) => () => {
      const backup = stage;
      stage = s;
      const res = act();
      stage = backup;
      return res;
    };
    const tail = (act) => () => {
      const backup = isTail;
      isTail = true;
      const res = act();
      isTail = backup;
      return res;
    };
    const nonTail = (act) => () => {
      const backup = isTail;
      isTail = false;
      const res = act();
      isTail = backup;
      return res;
    };
    const indent = (act) => () => {
      const backup = indentation;
      indentation += 2;
      const res = act();
      indentation = backup;
      return res;
    };
    function semi() {
      put(";");
    }
    const par = (act) => () => {
      put("(");
      act();
      put(")");
    };
    const bind = (x, act) => () => {
      if (closed) {
        localVars.add(x);
      }
      ;
      const res = act();
      if (closed) {
        localVars.delete(x);
      }
      return res;
    };
    const jLet = (x, closed2, t3, u) => () => {
      if (isTail) {
        put("const " + x + " = ");
        indent(nonTail(t3))();
        semi();
        newl();
        tail(bind(x, u))();
      } else {
        put("((" + x + ") => ");
        par(nonTail(bind(x, u)))();
        put(")(");
        nonTail(t3)();
        put(")");
      }
    };
    const jSeq = (t3, u) => () => {
      if (isTail) {
        indent(nonTail(t3))();
        semi();
        newl();
        tail(u)();
      } else {
        put("((_) => ");
        par(nonTail(u))();
        put(")(");
        nonTail(t3)();
        put(")");
      }
    };
    const jTuple = (xs) => () => {
      if (xs.length === 0) {
        put("()");
      } else {
        put("(");
        xs[0]();
        xs.slice(1, xs.length).forEach((act) => {
          put(", ");
          act();
        });
        put(")");
      }
    };
    const jReturn = (t3) => () => {
      if (isTail) {
        put("return ");
        nonTail(t3)();
      } else {
        t3();
      }
    };
    const jLam = (xs, closed2, t3) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(" => {");
        xs.forEach((x) => localVars.add(x));
        tail(t3)();
        xs.forEach((x) => localVars.delete(x));
        put("}");
      })();
    };
    const jLamExp = (xs, closed2, t3) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(" => ");
        nonTail(t3)();
      })();
    };
    const cApp = (t3, u) => () => {
      jReturn(() => {
        par(t3)();
        put("._1");
        par(u)();
      })();
    };
    const jApp = (t3, args) => () => {
      jReturn(() => {
        t3();
        jTuple(args)();
      })();
    };
    const cRun = (t3) => () => {
      jReturn(() => {
        t3();
        put("()");
      })();
    };
    const jClosure = (env, x, closed2, t3) => () => {
      if (env.length === 0) {
        jLam([x], closed2, t3)();
      } else {
        jLamExp(env, closed2, jLam([x], closed2, t3))();
      }
    };
    const jAppClosure = (t3, args) => () => {
      if (args.length === 0) {
        t3();
      } else {
        t3();
        jTuple(args)();
      }
    };
    const oRec = (ts) => () => {
      const arr = Array.from(ts);
      function go(i) {
        if (i == arr.length) {
          return;
        } else if (arr.length !== 0 && i == arr.length - 1) {
          put("[");
          put(arr[i][0]);
          put(", ");
          nonTail(() => oeval(arr[i][1]))();
          put("]");
        } else {
          put("[");
          put(arr[i][0]);
          put(", ");
          nonTail(() => oeval(arr[i][1]))();
          put("], ");
          return go(i + 1);
        }
      }
      go(0);
    };
    const openVar = (x) => x + "o";
    const oevalVar = (x) => () => {
      if (localVars.has(x)) {
        return jReturn(str(x))();
      } else {
        return jApp(str("Var_"), [strLit(x)])();
      }
    };
    function oeval(top) {
      switch (top.tag) {
        case _Var:
          return oevalVar(top._1)();
        case _CSP:
          return jApp(str("CSP_"), [str("csp_[" + top._1 + "]"), strLit(top._2)])();
        case _Let: {
          if (stage === 0) {
            return jLet(top._1, false, () => oeval(top._2), () => oeval(top._3))();
          } else {
            return jApp(str("Let_"), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])();
          }
        }
        case _Lam: {
          return jApp(str("Lam_"), [strLit(top._1), jLam([top._1], false, () => oeval(top._2))])();
        }
        case _LiftedLam:
          throw new Error("impossible");
        case _App: {
          if (stage === 0) {
            return jApp(str("app_"), [() => oeval(top._1), () => oeval(top._2)])();
          } else {
            return jApp(str("App_"), [() => oeval(top._1), () => oeval(top._2)])();
          }
        }
        case _Quote:
          return jApp(str("quote_"), [inStage(stage + 1, () => oeval(top._1))])();
        case _Splice: {
          if (stage === 0) {
            return jApp(str("codegenOpen_"), [() => oeval(top._1)])();
          } else {
            return inStage(stage - 1, jApp(str("splice_"), [() => oeval(top._1)]))();
          }
        }
        case _Return:
          return jApp(str("Return_"), [() => oeval(top._1)])();
        case _Bind:
          return jApp(str("Bind_"), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])();
        case _Seq:
          return jApp(str("Seq_"), [() => oeval(top._1), () => oeval(top._2)])();
        case _New:
          return jApp(str("New_"), [() => oeval(top._1)])();
        case _Write:
          return jApp(str("Write_"), [() => oeval(top._1), () => oeval(top._2)])();
        case _Read:
          return jApp(str("Read_"), [() => oeval(top._1)])();
        case _Log:
          return jApp(str("Log_"), [strLit(top._1)])();
        case _ReadNat:
          return jApp(str("ReadNat_"), [])();
        case _PrintNat:
          return jApp(str("PrintNat_"), [() => oeval(top._1)])();
        case _Rec:
          return jApp(str("Rec_"), [() => {
            put("new Map([");
            oRec(top._1)();
            put("])");
          }])();
        case _Proj: {
          if (stage === 0) {
            return jApp(str("proj_"), [() => oeval(top._1), strLit(top._2)])();
          } else {
            return jApp(str("Proj_"), [() => oeval(top._1), strLit(top._2)])();
          }
        }
        case _Suc: {
          if (stage === 0) {
            return jApp(str("suc_"), [() => oeval(top._1)])();
          } else {
            return jApp(str("Suc_"), [() => oeval(top._1)])();
          }
        }
        case _NatElim: {
          if (stage === 0) {
            return jApp(str("natElim_"), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])();
          } else {
            return jApp(str("NatElim_"), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])();
          }
        }
        case _NatLit: {
          const s = top._1.toString();
          return jApp(str("CSP_"), [str(s), strLit(s)])();
        }
      }
    }
    put("() => {\n");
    oeval(t2);
    put(";\n}");
    return build();
  }
  switch (t.tag) {
    case _CSP: {
      const res = codegenClosed_(t._1, loc);
      return CSP_(res, "");
    }
    case _Quote: {
      const { _1: t2, _2: cspArray } = quote(t._1);
      const source = emitCode(t2);
      return evalCodeGenOpen_(source, cspArray, loc);
    }
    default: {
      return Splice_(t);
    }
  }
}
function cSuc_(t) {
  if (typeof t === "number") {
    return t + 1;
  } else {
    throw new Error("impossible");
  }
}
function cNatElim_(s, z, n) {
  if (n === 0) {
    return z;
  } else if (n > 0) {
    const m = n - 1;
    return s._1(m)._1(cNatElim_(s, z, m));
  } else {
    throw new Error("impossible");
  }
}
function app_(t, u) {
  if (t.tag === _CSP) {
    const v1 = (
      /** @type{{_1: (v:Closed) => Closed, _2: (v:Open) => Open}} */
      t._1
    );
    if (u.tag === _CSP) {
      return CSP_(v1._1(u._1), "");
    } else {
      return v1._2(u);
    }
  } else if (t.tag === _Lam) {
    return t._2(u);
  } else {
    return App_(t, u);
  }
}
function splice_(t) {
  if (t.tag == _CSP) {
    return (
      /** @type{Open} */
      t._1
    );
  } else if (t.tag === _Quote) {
    return t._1;
  } else {
    return Splice_(t);
  }
}
function quote_(t) {
  if (t.tag === _Splice) {
    return t._1;
  } else {
    return Quote_(t);
  }
}
function proj_(t, x) {
  if (t.tag === _CSP) {
    return CSP_(t._1[x], "");
  } else if (t.tag === _Rec) {
    return (
      /** @type{Open} */
      t._1.get(x)
    );
  } else {
    return Proj_(t, x);
  }
}
function natElim_(s, z, n) {
  function go(n2) {
    if (n2 === 0) {
      return z;
    } else if (n2 > 0) {
      const m = n2 - 1;
      return app_(app_(s, CSP_(m, "")), go(m));
    } else {
      throw new Error("impossible");
    }
  }
  if (n.tag === _CSP) {
    return go(n._1);
  } else if (n.tag == _Suc) {
    return app_(app_(s, n._1), natElim_(s, z, n._1));
  } else {
    return NatElim_(s, z, n);
  }
}
function suc_(t) {
  if (t.tag === _CSP) {
    return CSP_(t._1 + 1, "");
  } else {
    return Suc_(t);
  }
}
function log_(s) {
  console.log(s);
  return {};
}
function readNat_() {
  const str = reader_.prompt();
  const num = parseFloat(str);
  const n = Math.round(num);
  if (!(num === n)) {
    throw new Error("Non-integral number");
  } else if (num < 0) {
    throw new Error("negative number");
  } else {
    return n;
  }
}
function printNat_(n) {
  console.log(n);
  return {};
}
