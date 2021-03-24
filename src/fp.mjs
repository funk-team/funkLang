import $_ from "sanctuary-def";
import S_ from "sanctuary";
import * as F_ from "fluture";
import * as FlutureTypes from "fluture-sanctuary-types";

const env = $_.env;

export const S = S_;
export const F = F_;
export const $ = $_;
export const Future = FlutureTypes.FutureType;

// helpers
export const log = tag => data => {
    console.log(`[${tag}]`, data);
    return data;
};

export const def = $.create({ checkTypes: true, env });
