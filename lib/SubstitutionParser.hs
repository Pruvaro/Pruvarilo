-- |
-- This module provides a simple substitution parser syntax,
-- used in the "Config" module as means of representing
-- shelll commands.
--
-- It is easier to explain the basic functionality with an
-- example. Then, we can focus on the details.
--
-- The main function in this module is 'subst'. It replaces
-- the any occurences of @%n@, where @n@ is a non-negative
-- integer, with the @n@-entry (with the index starting at
-- @0@) of a substitution list. If that list terminates
-- before that index, then @%n@ is replaced with @""@ (an
-- empty string). For example, running  
--
-- > subst "%0 %1! %1 %0" ["Hello", "World"]
--
-- results in
--
-- > "Hello World! World Hello"
--
-- Now, if we slightly modify this example with  
--
-- > subst "%0 %1! %1 %0 %2" ["Hello", "World"]
--
-- then, since there is nothing in index position @2@, the
-- result is just
-- 
-- > "Hello World! World Hello "
--
-- notice the space after the last word.
--
-- Some extra details: Suppose we wanted to make a 'String'
-- which, when given the substitution list @[\"Hello\"]@,
-- results in
--
-- > "Hello2"
--
-- One first attempt would be to write
--
-- > "%02"
--
-- but that is obviously not going to work, since, when
-- looking for replacement tokens (things of type @%n@, where
-- @n@ is a non-negative integer), we will replace @%02%@
-- with the @02@-th, that is, the third entry (i.e. entry with
-- index @2@) of the substitution list. Thus,
--
-- > subst "%02" ["Hello"] == ""
-- 
-- as there is nothing with index @2@.
--
-- Another attempt would be  
--
-- > "%0 2"
--
-- but here, we have a space between the token and @2@, so
-- the substitution result is
--
-- > subst "%0 2" ["Hello"] == "Hello 2"
--
-- Not what we wanted. As a solution, we add a token which
-- __always__ replaces to @""@. That token is @%-@. So, here,
-- we can write
--
-- > "%0%-2"
--
-- which will substitute to
--
-- > subst "%0%-2" ["Hello"] == "Hello2"
--
-- Now, what if we want to add an actual percent sign @%@?
-- There are two main ways. If whatever follow the sign is
-- neither a digit nor @-@, then you can just write it as
-- normal. That is,
--
-- > subst "%APPDATA%" ["Hello"] == "%APPDATA%"
--
-- since, for the first @%@, it is followed by @A@ (not a
-- digit nor @-@), and the second @%@ is not followed by
-- anything, so it also does not get replaced. But, if
-- one wanted to get
--
-- > "Hello %1"
--
-- when using the replacement list @[\"Hello\"]@, then, we
-- can use __two percent signs__ @%@ where we want just
-- one,  
--
-- > subst "%0 %%1" ["Hello"] == "Hello %1"
--
-- The parsing goes from left to right. So, notice how  
--
-- > subst "%0 %%1" ["Hello", "World"]
--
-- parses to
--
-- > "Hello %1"
--
-- that is, giving precedence to @%%@ in @%%1@, which gets
-- read as just @%@, instead of  
--
-- > "Hello %World"
-- 
-- which would be the case if it gave precedence to @%1@ in
-- @%%1@, replacing it with the index @1@ entry @\"World\"@.
module SubstitutionParser
  ( subst
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import qualified Safe as S
  ( atMay
  )

import qualified Text.Read as TR
  ( readMaybe
  )

-- MAIN FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The main substitution function.
subst
  :: String
  -- ^
  -- The 'String' with substitution tokens.
  -> [String]
  -- ^
  -- The list of substitution 'String's.
  -> String
subst str subs = percentDetector "" str subs


-- AUXILIAR FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This function goes through its second argument
-- 'String', accumulating the head on the first argument
-- (it ends up with a reversed list in the accumulator
-- argument, for efficiency reasons). However, if at the
-- head of the list its trying to read, it gets a @%@, it
-- will decide what to do, based on a substitution list.
percentDetector
  :: String
  -- ^
  -- The accumulator list (generally empty at first call).
  -> String
  -- ^
  -- The "after" list, containing the characters that have yet
  -- to be consumed.
  -> [String]
  -- ^
  -- The list of subsitutions.
  -> String
-- If we have no more characters to read, just reverse the
-- accumulator list and return that.
percentDetector bf [] _ = reverse bf
-- Similarly, even if we find '%' as a last character, it
-- does not require a substitution, so just accumulate and
-- return the reverse of everything.
percentDetector bf [c] _ = reverse (c : bf)
-- Here, there are at least two characters yet to be read,
-- which is enough for a token.
percentDetector bf (c : c' : aft) subs =
  -- If the first character is not a percent sign, just
  -- accumulate at the head of the accumulator (causing an
  -- inversion in the list order, which we fix in the end) and
  -- move on, reading the rest.
  if (c /= '%')
  then
    percentDetector (c : bf) (c' : aft) subs
  else
    -- Now, if it is a percent sign, we must see what follows.
    case c' of
      '-' ->
        -- Here we found the "replace with nothing" token.
        -- So, we just remove both c and c' (since they
        -- have already been consumed) and move on consuming
        -- the list.
        percentDetector bf aft subs
      '%' ->
        -- Here we found the "replace with '%' token. So, we
        -- just accumulate a single '%' at the top of the
        -- accumulator and continue.
        percentDetector ('%' : bf) aft subs
      otherwise ->
        -- Here we ruled out the two special tokens. So,
        -- we either have '%n', for some number 'n', or
        -- '%k%, for some other character %k%. Because 'n'
        -- might be several digits long, we use the auxiliar
        -- function 'munchNums' to read through the things
        -- following the percent sign.
        let
          -- 'numStr' is either a string of digits, or an
          -- empty string. If it is empty, the munching did
          -- get get any digits at all, so we are in the '%k'
          -- case, where @k@ is a non-digit character.
          --
          -- 'aft'' is what is still left to process, after
          -- we munched the number part.
          (numStr, aft') = munchNums "" (c' : aft)
        in
          if (numStr == "")
          then
            -- An empty 'numStr' means what follows '%' is
            -- a non-digit character. The special token ones
            -- ('-' and '%') have already been looked at, so
            -- we know they are not the ones. Here, we should
            -- interpret '%' as just '%', and accumulate them
            -- (with reversed order), then continue processing.
            percentDetector (c' : c : bf) aft subs
          else
            let
              -- We try to read it. Normally, we should
              -- never get the 'Nothing' case.
              mbNum :: Maybe Int
              mbNum = TR.readMaybe numStr
            in
              case mbNum of
                Nothing ->
                  -- Should never happen. However, if it
                  -- does, we will default to continuing
                  -- with whatever 'munchNums' tells us
                  -- follows the "number".
                  percentDetector bf aft' subs
                Just num ->
                  -- We now must look if the thing at that
                  -- index really exists.
                  let
                    mbSubStr :: Maybe String
                    mbSubStr = subs `S.atMay` num
                  in
                    case mbSubStr of
                      Nothing ->
                        -- Nothing at that index. Replace
                        -- with nothing.
                        percentDetector bf aft' subs
                      Just subStr ->
                        -- Reverse the substitution string
                        -- and prepend it.
                        percentDetector
                          ((reverse subStr) ++ bf)
                          aft'
                          subs
  where
    -- Auxiliar function which reads a string, accumulates
    -- only digits, and returns the digits and the list
    -- of what followed those digits.
    munchNums
      :: String
      -- ^
      -- The digit accumulator (generally empty on first call).
      -> String
      -- ^
      -- The things yet to be looked at.
      -> (String, String)
      -- ^
      -- The digits found, and what remains to be processed
      -- following the digits.
    -- As with the main functon, the accumulator gets the
    -- orders reversed for efficiency. The second entry is
    -- @[]@, since there is nothing left to process.
    munchNums acc [] = (reverse acc, [])
    munchNums acc (k : ks) =
      if (k `elem` ['0','1','2','3','4','5','6','7','8','9'])
      then
        -- We found a digit. Accumulate on the head of the
        -- accumulator (so the accumulator will have to be
        -- reversed in the end) and look at the rest.
        munchNums (k : acc) ks
      else
        -- Not a digit. So, the number has been totally read,
        -- and we must return everything else which is not
        -- a number, but first, reverse the accumulator.
        (reverse acc, k : ks)
