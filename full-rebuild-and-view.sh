#!/bin/sh

stack build && stack exec site clean && stack exec site watch
