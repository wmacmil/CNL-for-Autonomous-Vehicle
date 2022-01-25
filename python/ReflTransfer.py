#!/usr/bin/env python
# coding: utf-8

# # Part I: preliminaries

# *The first section is a general introduction to the PGF library and a few helpful functions. If you know the basics already, feel free to skip to [Part II](#Part-II:-Manipulating-syntax-trees).*
# 
# You should have the file `MiniLang.pgf` in the same directory where you run this notebook.
# 
# First, we import the PGF library.

# In[2]:


import pgf


# After importing the PGF library, we read the .pgf file and store it in a variable `gr`.

# In[4]:


gr = pgf.readPGF("MiniLang.pgf")
type(gr)


# We can access the concrete syntaxes in `gr.languages`, which is a dictionary. English is found in `MiniLangEng`.
# 
# Why? Because we wrote in the GF grammar `concrete MiniLangEng of MiniLang`. Thus the concrete syntax is named `MiniLangEng` also in the PGF.

# In[5]:


eng = gr.languages["MiniLangEng"]


# With the concrete syntax `eng`, we can e.g. *parse* a sentence:

# In[16]:


i = eng.parse("the star is big")
j = eng.parse("every tree is small and every man is big")
prob,expr = next(j)
print(expr)


# In[18]:


eng.linearize(expr)


# Now we *embed* the grammar, so that we can use the GF functions (`DetCN` etc.) as Python functions

# In[20]:


gr.embed("MiniLang")


# In[21]:


from MiniLang import *


# We import * from MiniLang just so that we don't need to write `MiniLang.DetCN(MiniLang.the_Det, MiniLang.UseN(MiniLang.star_N))`.
# 
# Check out all the functions in scope:

# In[22]:


dir()


# Now we can form trees out of the GF constructors, in Python with Python's syntax for function application. Compare the two syntaxes:
# 
# 
# * GF
# ```
# DetCN the_Det (UseN star_N)
# ```
# 
# * Python
# ```
# DetCN(the_Det, UseN(star_N))
# ```
# 
# We can create a full sentence like this and call it `bigStar`. Its type is `pgf.Expr`.

# In[23]:


bigStar = UsePresCl(PPos, PredVP(DetCN(the_Det, UseN(star_N)), UseAP(PositA(big_A))))
type(bigStar)


# If we print `bigStar`, we'll see how it looks like with GF's syntax for function application.

# In[24]:


print(bigStar)


# We can also linearise `bigStar` with the concrete syntax `eng`.

# In[25]:


eng.linearize(bigStar)


# Somewhat more interesting function is `unpack`. Let's try it on `bigStar`:

# In[36]:


x = bigStar.unpack()
y = (x,x)
y[0]
y[0][1][1].unpack()


# `unpack` returns a tuple of `(String, [Expr])`, i.e. the name of the constructor and a list of its subtrees. We can further call `unpack` to the subtrees to get their names and subtrees.

# In[15]:


topName, children = bigStar.unpack()
(topName, [child.unpack() for child in children])


# If you think it would be fun, you can try to write one function that returns the names of all the constructors from an expression, all in one list. Use `unpack`, not the string representation of the tree. (Hint: you can search for tree traversal algorithms.) 
# 
# If you don't think this sounds like fun, skip to the next section! 

# In[16]:


def allnames(expr):
    # Write an actual implementation here
    return "[UsePresCl, PPos, PredVP, DetCN, the_Det, UseN, star_N, UseAP, PositA, big_A] in any order"
        
allnames(bigStar)


# # Part II: Manipulating syntax trees

# Now we get to the part where we do some syntactic transfer on GF trees.
# 
# The GF grammar generates sentences such as *I like me*, *you see you*, *John asks John*.  
# We will transform them into reflexive versions: *I like myself*, *you see yourself*, *John asks himself*.
# 
# The usage of the function should look like this:
# 
# ```python
# > f("I see you")
# I see you  
# 
# > f("I see me")
# I see myself
# ```

# In order for a sentence to be transformed, its subject and object must be the same. Thus we need to pattern match at the `Cl` level: only then we have access to both subject and object. So this is the smallest tree we can look at:
# 
# ```haskell
# PredVP (UsePron i_Pron) (ComplV2 see_V2 (UsePron i_Pron)))
# ```
# 
# Let's define a function that looks at a `Cl`. We know already how to use `unpack`, now all we need to do is to check the names of the subtrees and decide if we go further with the transformation.

# In[17]:


def toReflexiveCl(cl):
    '''Analysing Cl'''
    clFunName, clChildren = cl.unpack()
    if clFunName=="PredVP":
        # When we match a function name, 
        # we know which and how many arguments it has.
        subj = clChildren[0]
        vp = clChildren[1]
        vpFunName, vpChildren = vp.unpack()
        if vpFunName=="ComplV2": # Only consider VPs that have an object
            v2 = vpChildren[0]
            obj = vpChildren[1]
            if subj==obj:
                reflVP = ReflV2(v2) # Remember: ReflV2 is now a Python function, thanks to gr.embed(MiniLang)
                return PredVP(subj, reflVP) # Old subject, new reflexive VP
    return cl


# This function would now work if we apply it to a tree of type `Cl`. But when we parse a sentence, we don't get a `Cl` but an `Utt`; that's because `Utt` is the start category of the grammar. Thus we need more functions to transform `S` and `Utt`.

# In[18]:


def toReflexiveS(sent):
    '''Analysing S'''
    sFunName, sChildren = sent.unpack()
    if sFunName=="UsePresCl":
        pol = sChildren[0] # polarity
        cl = sChildren[1] # clause
        return UsePresCl(pol, toReflexiveCl(cl))
    elif sFunName=="CoordS":
        conj = sChildren[0]
        s1 = sChildren[1]
        s2 = sChildren[2]
        return CoordS(conj, toReflexiveS(s1), toReflexiveS(s2))
    else: return sent
        
def toReflexive(utt):
    '''Top layer: analysing the start category Utt.'''
    uttFunName, uttChildren = utt.unpack()
    if uttFunName=="UttS": # there chould be a reflexive in the child of type S
        s = uttChildren[0]
        return UttS(toReflexiveS(s))
    else: return utt # The other constructor is UttNP, and it cannot contain a reflexive.


# Now we can define a transfer function that takes a sentence, parses it, reflexivises the syntax tree (if applicable) and linearises the transformed tree.

# In[19]:


def transfer(str):
    i = eng.parse(str)
    prob,expr = next(i)
    print(eng.linearize(toReflexive(expr)))
    
transfer("I see you")
transfer("I see me")


# In[ ]:




