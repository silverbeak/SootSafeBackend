package com.sootsafe.engine

import com.sootsafe.model.{Model, ModelBuilder}
import com.sootsafe.serializers.NodeSerializer
import com.sootsafe.valuetable.ValueResolver
import org.json4s.native.Serialization.read
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.{Matchers, WordSpecLike}

class PressureLossTest extends WordSpecLike with Matchers {

  implicit val formats: Formats = DefaultFormats + NodeSerializer

  private val anotherJson =
    """
      |{ "class": "go.GraphLinksModel",
      |  "copiesArrays": true,
      |  "copiesArrayObjects": true,
      |  "linkKeyProperty": "key",
      |  "linkFromPortIdProperty": "fid",
      |  "linkToPortIdProperty": "tid",
      |  "nodeDataArray": [
      |{"key":1, "ssInfo":{"nodeType":"outlet"}, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -77.99999999999999"},
      |{"key":22, "angle":90, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-147.3333333333332 -17.999999999999986", "ssInfo":{"nodeType":"t-pipe", "capacity":51, "dimension":{"diameter":160}}},
      |{"key":-3, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 22.000000000000014", "ssInfo":{"nodeType":"areaIncrement", "capacity":34, "dimension":{"diameter":160}}},
      |{"key":-13, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 22.000000000000014", "ssInfo":{"nodeType":"areaIncrement", "capacity":68, "dimension":{"diameter":200}}},
      |{"key":-4, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 42.000000000000014", "ssInfo":{"nodeType":"pipe", "dimension":{"diameter":125, "length":2700}, "capacity":34}},
      |{"key":-5, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 62.000000000000014", "ssInfo":{"nodeType":"fireCell", "targetCell":true}},
      |{"key":-17, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 62.000000000000014", "ssInfo":{"nodeType":"box"}},
      |{"key":-6, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"pipe", "capacity":51, "dimension":{"diameter":160, "length":2700}}},
      |{"key":-15, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"pipe", "capacity":156, "dimension":{"diameter":200, "length":6000}}},
      |{"key":-8, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"t-pipe", "capacity":68, "dimension":{"diameter":160}}},
      |{"key":-14, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"t-pipe", "capacity":156, "dimension":{"diameter":200}}},
      |{"key":-11, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"bend", "capacity":68, "dimension":{"diameter":160, "angle":90}}},
      |{"key":-12, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"bend", "capacity":68, "dimension":{"diameter":160, "angle":90}}},
      |{"key":-16, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"bend", "capacity":156, "dimension":{"diameter":200, "angle":90}}},
      |{"key":-10, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-137.3333333333332 -57.999999999999986", "ssInfo":{"nodeType":"pipe", "capacity":68, "dimension":{"diameter":160, "length":4000}}},
      | ],
      |  "linkDataArray": [
      |{"from":-3, "to":22, "fid":"U6", "tid":"U0", "key":-2},
      |{"from":-4, "to":-3, "fid":"U6", "tid":"U2", "key":-3},
      |{"from":-5, "to":-4, "fid":"U6", "tid":"U2", "key":-4},
      |{"from":-17, "to":1, "fid":"U6", "tid":"U2", "key":-5},
      |{"from":-16, "to":-17, "fid":"U6", "tid":"U2", "key":-17},
      |{"from":-15, "to":-16, "fid":"U6", "tid":"U2", "key":-16},
      |{"from":-14, "to":-15, "fid":"U6", "tid":"U2", "key":-15},
      |{"from":-13, "to":-14, "fid":"U6", "tid":"U2", "key":-14},
      |{"from":-12, "to":-13, "fid":"U6", "tid":"U2", "key":-13},
      |{"from":-11, "to":-12, "fid":"U6", "tid":"U2", "key":-12},
      |{"from":-10, "to":-11, "fid":"U6", "tid":"U2", "key":-11},
      |{"from":-6, "to":-8, "fid":"U6", "tid":"U2", "key":-9},
      |{"from":-6, "to":22, "fid":"U2", "tid":"U4", "key":-6},
      |{"from":-8, "to":-10, "fid":"U2", "tid":"U4", "key":-10}
      | ]}""".stripMargin

  private val jsonString =
    """
      |{ "class": "go.GraphLinksModel",
      |  "copiesArrays": true,
      |  "copiesArrayObjects": true,
      |  "linkKeyProperty": "key",
      |  "linkFromPortIdProperty": "fid",
      |  "linkToPortIdProperty": "tid",
      |  "nodeDataArray": [
      |{"key":-2, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-75.33333333333354 -153.99999999999994", "ssInfo":{"nodeType":"pipe", "capacity":88.1}},
      |{"key":24, "angle":270, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-65.33333333333354 -113.99999999999993", "ssInfo":{"nodeType":"base"}},
      |{"key":-4, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-75.33333333333354 -73.99999999999991", "ssInfo":{"nodeType":"pipe", "capacity":"66.2"}},
      |{"key":-5, "angle":270, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-65.33333333333354 -33.999999999999915", "ssInfo":{"nodeType":"base"}},
      |{"key":-6, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-75.33333333333354 6.000000000000085", "ssInfo":{"capacity":"44", "nodeType":"pipe"}},
      |{"key":3, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-35.33333333333354 -113.99999999999991", "ssInfo":{"name":"FD8", "nodeType":"fireCell"}},
      |{"key":-8, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-35.33333333333354 -33.999999999999915", "ssInfo":{"nodeType": "fireCell", "name":"FD7"}},
      |{"key":11, "geo":"F1 M0 40 L0 30 Q0 0 30 0 L40 0 40 20 30 20 Q20 20 20 30 L20 40z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U2", "spot":"0.25 1 0.25 -0.5"} ], "loc":"-125.33333333333354 -183.99999999999997", "ssInfo":{"comment":"Böj", "nodeType":"base"}},
      |{"key":21, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-75.33333333333354 -183.99999999999994", "ssInfo":{"comment":"7", "name":"Horizontal T-pipe", "nodeType":"tpipe"}},
      |{"key":22, "angle":90, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-145.33333333333354 -113.99999999999997", "ssInfo":{"nodeType":"base"}},
      |{"key":1, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-135.33333333333354 -153.99999999999997", "ssInfo":{"capacity":"68", "nodeType":"pipe", "dimension":"160"}},
      |{"key":-14, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-135.33333333333354 -73.99999999999997", "ssInfo":{"nodeType":"pipe", "capacity":"51", "dimension":"160"}},
      |{"key":-15, "angle":90, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-145.33333333333354 -33.99999999999997", "ssInfo":{"capacity":"", "nodeType":"base"}},
      |{"key":-16, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-135.33333333333354 6.000000000000028", "ssInfo":{"capacity":"64", "nodeType":"pipe", "dimension":"160"}},
      |{"key":-18, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-175.33333333333354 -113.99999999999997", "ssInfo":{"name":"FD4", "nodeType":"fireCell"}},
      |{"key":-19, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-175.33333333333354 -33.99999999999997", "ssInfo":{"name":"FD3", "nodeType":"fireCell"}},
      |{"key":-21, "angle":180, "geo":"F1 M0 40 L0 30 Q0 0 30 0 L40 0 40 20 30 20 Q20 20 20 30 L20 40z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U2", "spot":"0.25 1 0.25 -0.5"} ], "loc":"-25.33333333333354 -203.99999999999994", "ssInfo":{"nodeType":"base"}},
      |{"key":-22, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-15.333333333333542 -233.99999999999994", "ssInfo":{"comment":"Låda", "name":"71", "nodeType":"outlet", "capacity":156.444, "dimension":"200"}},
      |{"key":-20, "angle":90, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-145.33333333333354 46.00000000000003", "ssInfo":{"nodeType":"base"}},
      |{"key":-23, "angle":270, "geo":"F1 M0 0 L60 0 60 20 50 20 Q40 20 40 30 L40 40 20 40 20 30 Q20 20 10 20 L0 20z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U4", "spot":"0 0.25 0.5 0.25"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-65.33333333333354 46.000000000000085", "ssInfo":{"nodeType":"base"}},
      |{"key":13, "angle":180, "geo":"F1 M0 40 L0 30 Q0 0 30 0 L40 0 40 20 30 20 Q20 20 20 30 L20 40z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U2", "spot":"0.25 1 0.25 -0.5"} ], "loc":"-145.33333333333354 116.00000000000003", "ssInfo":{"nodeType":"base"}},
      |{"key":14, "angle":270, "geo":"F1 M0 40 L0 30 Q0 0 30 0 L40 0 40 20 30 20 Q20 20 20 30 L20 40z", "ports":[ {"id":"U0", "spot":"1 0.25 -0.5 0.25"},{"id":"U2", "spot":"0.25 1 0.25 -0.5"} ], "loc":"-65.33333333333354 116.00000000000009", "ssInfo":{"nodeType":"base"}},
      |{"key":-24, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-175.33333333333354 126.00000000000003", "ssInfo":{"name":"FD1", "nodeType":"fireCell", "targetCell": true}},
      |{"key":-25, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-175.33333333333354 46.00000000000003", "ssInfo":{"name":"FD2", "nodeType":"fireCell"}},
      |{"key":-26, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-35.33333333333354 46.000000000000085", "ssInfo":{"name":"FD6", "nodeType":"fireCell"}},
      |{"key":-27, "angle":90, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "loc":"-35.33333333333354 126.00000000000009", "ssInfo":{"name":"FD5", "nodeType":"fireCell"}},
      |{"key":-28, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-135.33333333333354 86.00000000000003", "ssInfo":{"nodeType":"pipe", "capacity":"17", "dimension":"125"}},
      |{"key":-29, "geo":"F1 M0 0 L20 0 20 20 0 20z", "ports":[ {"id":"U6", "spot":"0.5 0 0 0.5"},{"id":"U2", "spot":"0.5 1 0 -0.5"} ], "fill":"rgba(128, 0, 128, 0.5)", "loc":"-75.33333333333354 86.00000000000009", "ssInfo":{"capacity":"22", "nodeType":"pipe"}}
      | ],
      |  "linkDataArray": [
      |{"from":24, "to":-2, "fid":"U0", "tid":"U2", "key":-2},
      |{"from":-4, "to":24, "fid":"U6", "tid":"U4", "key":-3},
      |{"from":-5, "to":-4, "fid":"U0", "tid":"U2", "key":-4},
      |{"from":-6, "to":-5, "fid":"U6", "tid":"U4", "key":-5},
      |{"from":3, "to":24, "fid":"U2", "tid":"U2", "key":-7},
      |{"from":-8, "to":-5, "fid":"U2", "tid":"U2", "key":-8},
      |{"from":21, "to":-2, "fid":"U2", "tid":"U6", "key":-19},
      |{"from":11, "to":21, "fid":"U0", "tid":"U4", "key":-20},
      |{"from":1, "to":11, "fid":"U6", "tid":"U2", "key":-11},
      |{"from":22, "to":1, "fid":"U4", "tid":"U2", "key":-12},
      |{"from":-14, "to":22, "fid":"U6", "tid":"U0", "key":-13},
      |{"from":-15, "to":-14, "fid":"U4", "tid":"U2", "key":-14},
      |{"from":-16, "to":-15, "fid":"U6", "tid":"U0", "key":-15},
      |{"from":-18, "to":22, "fid":"U6", "tid":"U2", "key":-17},
      |{"from":-19, "to":-15, "fid":"U6", "tid":"U2", "key":-18},
      |{"from":-21, "to":21, "fid":"U0", "tid":"U0", "key":-22},
      |{"from":-22, "to":-21, "fid":"U2", "tid":"U2", "key":-23},
      |{"from":-20, "to":-16, "fid":"U4", "tid":"U2", "key":-21},
      |{"from":-23, "to":-6, "fid":"U0", "tid":"U2", "key":-24},
      |{"from":-24, "to":13, "fid":"U6", "tid":"U0", "key":-27},
      |{"from":-25, "to":-20, "fid":"U6", "tid":"U2", "key":-28},
      |{"from":-26, "to":-23, "fid":"U2", "tid":"U2", "key":-29},
      |{"from":-27, "to":14, "fid":"U2", "tid":"U2", "key":-30},
      |{"from":-28, "to":-20, "fid":"U6", "tid":"U0", "key":-25},
      |{"from":13, "to":-28, "fid":"U2", "tid":"U2", "key":-31},
      |{"from":-29, "to":-23, "fid":"U6", "tid":"U4", "key":-26},
      |{"from":14, "to":-29, "fid":"U0", "tid":"U2", "key":-32}
      | ]}
    """.stripMargin


  "PressureLossTest" must {

    "calculatePressureLoss" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val model = read[Model](anotherJson)
      val linkedModel = new ModelBuilder(model).buildModel()

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(linkedModel)
      val pressureLoss = pressureLossTable.foldLeft(0d)((agg, pl) => pl.pressureLoss + agg)
      pressureLossTable.length should be (14)
      pressureLoss should be (54.365969931577)
    }

  }
}
