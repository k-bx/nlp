"""Homework"""

import spacy


if __name__ == '__main__':
    nlp = spacy.load('en_core_web_lg')
    docs = [
        "We can but hope that everything will be fine.",
        "It's sad but true.",
        "Jack brings nothing but trouble.",
        "As we were talking, I realised how to solve the issue.",
        "This hot dog isn't as big as usual.",
        "This hot dog isn't as big as usual.",
        "This hot dog isn't as big as I expected.",
        "I work as a teacher.",
        "Let's do it this way!",
        "This is way too much!",
        "The prices are going down.",
        "Someone pushed him and he fell down the stairs.",
        "I’ve been feeling rather down lately.",
        "It's not easy to down a cup of coffee in one gulp.",
        "Bring a down jacket and a pair of gloves, and you'll be fine."]
    for line in docs:
        doc = nlp(line)
        print("")
        print("> {}".format(doc))
        for t in doc:
            print(t.text, t.lemma_, t.pos_, t.tag_, t.dep_, t.shape_, t.is_alpha, t.is_stop)
