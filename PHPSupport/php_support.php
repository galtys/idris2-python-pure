<?php
declare(strict_types=0);
mb_internal_encoding('UTF-8');

$GLOBALS['_idrisworld'] = null;
$GLOBALS['py_support_erased'] = null;
$GLOBALS['UNIT'] = null;
$GLOBALS['array_count'] = 0;
$GLOBALS['array_dict'] = [];

$GLOBALS['__tailRec'] = function($f, $ini) {
    $obj = $ini;
    while (true) {
        if ($obj['h_x'] === 0) {
            return $obj['a1'];
        } else {
            $obj = $f($obj);
        }
    }
};

function prim_getArgCount() {
    return $GLOBALS['argc'] ?? count($_SERVER['argv'] ?? []);
}
function prim_getArg($n) {
    $argv = $GLOBALS['argv'] ?? $_SERVER['argv'] ?? [];
    return $argv[$n] ?? '';
}

function print_obj($o) {
    fwrite(STDOUT, (string)$o);
}

function php_support_writeLine($f, $s) {
    fwrite($f, $s);
    return null;
}

function php_support_isNone($x) {
    return ($x === null) ? 1 : 0;
}

function php_support_fastUnpack($x) {
    $acc = ['h_x' => 0];
    $chars = mb_str_split((string)$x, 1, 'UTF-8');
    foreach (array_reverse($chars) as $c) {
        $acc = ['a1' => $c, 'a2' => $acc];
    }
    return $acc;
}

function php_support_fastPack($x) {
    $result = '';
    while (array_key_exists('a1', $x)) {
        $result .= $x['a1'];
        $x = $x['a2'];
    }
    return $result;
}

function fastConcat($xs) {
    $result = '';
    while (array_key_exists('a1', $xs)) {
        $result .= $xs['a1'];
        $xs = $xs['a2'];
    }
    return $result;
}

function newArray($s, $v) {
    $GLOBALS['array_count']++;
    $GLOBALS['array_dict'][$GLOBALS['array_count']] = array_fill(0, (int)$s, $v);
    return $GLOBALS['array_count'];
}

function __lazy($thunk) {
    $evaluated = false;
    $result = null;
    return function() use (&$evaluated, &$result, &$thunk) {
        if (!$evaluated) {
            $result = $thunk();
            $evaluated = true;
            $thunk = null;
        }
        return $result;
    };
}

function _prim_newIORef($v) {
    $o = new stdClass();
    $o->valuePrim = $v;
    return $o;
}
function _prim_readIORef($r)     { return $r->valuePrim; }
function _prim_writeIORef($r, $v){ $r->valuePrim = $v; return null; }

function _crashExp($x) {
    throw new \Exception((string)$x);
}

function _bigIntOfString($s) { return intval($s); }
function _numberOfString($s) { return floatval($s); }
function _intOfString($s)    { return intval(floatval($s)); }

function _truncToChar($x) {
    $xi = (int)$x;
    if (($xi >= 0 && $xi <= 55295) || ($xi >= 57344 && $xi <= 1114111)) {
        return mb_chr($xi, 'UTF-8') ?: "\0";
    }
    return "\0";
}

function _strReverse($x) {
    $chars = mb_str_split((string)$x, 1, 'UTF-8');
    return implode('', array_reverse($chars));
}

function _substr($offset, $len, $x) {
    return mb_substr((string)$x, (int)$offset, (int)$len, 'UTF-8');
}

function __prim_idris2js_array($x) {
    $result = [];
    while (array_key_exists('a1', $x)) {
        $result[] = $x['a1'];
        $x = $x['a2'];
    }
    return $result;
}

function __prim_js2idris_array($x) {
    $acc = ['h_x' => 0];
    foreach (array_reverse($x) as $item) {
        $acc = ['a1' => $item, 'a2' => $acc];
    }
    return $acc;
}

function __prim_stringIteratorNew($_str)                   { return 0; }
function __prim_stringIteratorToString($x, $str, $it, $f) { return $f(mb_substr($str, $it)); }
function __prim_stringIteratorNext($str, $it)              { return null; }

/* ---- integer truncation ---- */
function _truncInt8($x)   { $r = (int)$x & 0xff;               return ($r >= 0x80)   ? $r - 0x100             : $r; }
function _truncInt16($x)  { $r = (int)$x & 0xffff;             return ($r >= 0x8000) ? $r - 0x10000           : $r; }
function _truncInt32($x)  { return (int)$x & 0xffffffff; }
function _truncBigInt64($x){ $r = (int)$x; return $r; }

function _truncUInt8($x)  { return (int)$x & 0xff; }
function _truncUInt16($x) { return (int)$x & 0xffff; }
function _truncUInt32($x) { return (int)$x & 0xffffffff; }
function _truncUInt64($x) { return (int)$x; }

function _div($a, $b) {
    $q = intval($a / $b);
    $r = $a % $b;
    if ($r < 0) { return $b > 0 ? $q - 1 : $q + 1; }
    return $q;
}
function _mod($a, $b) {
    $r = $a % $b;
    if ($r < 0) { return $b > 0 ? $r + $b : $r - $b; }
    return $r;
}

function _add8s($a,$b)  { return _truncInt8($a+$b); }
function _sub8s($a,$b)  { return _truncInt8($a-$b); }
function _mul8s($a,$b)  { return _truncInt8($a*$b); }
function _div8s($a,$b)  { return _truncInt8(_div($a,$b)); }
function _shl8s($a,$b)  { return _truncInt8($a<<$b); }
function _shr8s($a,$b)  { return _truncInt8($a>>$b); }

function _add16s($a,$b) { return _truncInt16($a+$b); }
function _sub16s($a,$b) { return _truncInt16($a-$b); }
function _mul16s($a,$b) { return _truncInt16($a*$b); }
function _div16s($a,$b) { return _truncInt16(_div($a,$b)); }
function _shl16s($a,$b) { return _truncInt16($a<<$b); }
function _shr16s($a,$b) { return _truncInt16($a>>$b); }

function _add32s($a,$b) { return _truncInt32($a+$b); }
function _sub32s($a,$b) { return _truncInt32($a-$b); }
function _mul32s($a,$b) { return _truncInt32($a*$b); }
function _div32s($a,$b) { return _truncInt32(_div($a,$b)); }
function _shl32s($a,$b) { return _truncInt32($a<<$b); }
function _shr32s($a,$b) { return _truncInt32($a>>$b); }

function _add64s($a,$b) { return _truncBigInt64($a+$b); }
function _sub64s($a,$b) { return _truncBigInt64($a-$b); }
function _mul64s($a,$b) { return _truncBigInt64($a*$b); }
function _div64s($a,$b) { return _truncBigInt64(_div($a,$b)); }
function _shl64s($a,$b) { return _truncBigInt64($a<<$b); }
function _shr64s($a,$b) { return _truncBigInt64($a>>$b); }

function _add8u($a,$b)  { return ($a+$b) & 0xff; }
function _sub8u($a,$b)  { return ($a-$b) & 0xff; }
function _mul8u($a,$b)  { return ($a*$b) & 0xff; }
function _div8u($a,$b)  { return intval($a/$b); }
function _shl8u($a,$b)  { return ($a<<$b) & 0xff; }
function _shr8u($a,$b)  { return ($a>>$b) & 0xff; }

function _add16u($a,$b) { return ($a+$b) & 0xffff; }
function _sub16u($a,$b) { return ($a-$b) & 0xffff; }
function _mul16u($a,$b) { return ($a*$b) & 0xffff; }
function _div16u($a,$b) { return intval($a/$b); }
function _shl16u($a,$b) { return ($a<<$b) & 0xffff; }
function _shr16u($a,$b) { return ($a>>$b) & 0xffff; }

function _add32u($a,$b) { return _truncUInt32($a+$b); }
function _sub32u($a,$b) { return _truncUInt32($a-$b); }
function _mul32u($a,$b) { return _truncUInt32($a*$b); }
function _div32u($a,$b) { return _truncUInt32(intval($a/$b)); }
function _shl32u($a,$b) { return _truncUInt32($a<<$b); }
function _shr32u($a,$b) { return _truncUInt32($a>>$b); }

function _and32u($a,$b) { return _truncUInt32($a&$b); }
function _or32u($a,$b)  { return _truncUInt32($a|$b); }
function _xor32u($a,$b) { return _truncUInt32($a^$b); }

function _add64u($a,$b) { return ($a+$b); }
function _sub64u($a,$b) { return ($a-$b); }
function _mul64u($a,$b) { return ($a*$b); }
function _div64u($a,$b) { return intval($a/$b); }
function _shl64u($a,$b) { return ($a<<$b); }
function _shr64u($a,$b) { return ($a>>$b); }

function _addBigInt($a,$b)  { return $a+$b; }
function _subBigInt($a,$b)  { return $a-$b; }
function _mulBigInt($a,$b)  { return $a*$b; }
function _divBigInt($a,$b)  { return _div($a,$b); }
function _modBigInt($a,$b)  { return _mod($a,$b); }
function _shlBigInt($a,$b)  { return $a<<$b; }
function _shrBigInt($a,$b)  { return $a>>$b; }

function _strerror($e) {
    if (function_exists('posix_strerror')) { return posix_strerror((int)$e); }
    return "Error " . (int)$e;
}
