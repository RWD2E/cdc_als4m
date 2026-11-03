import tempfile
import unittest
from pathlib import Path
from gen_vs_json_utils_modular import ConceptRegistry, QueryFromJson, JsonBlockVS, json2ref, expand_range

ROOT=Path(__file__).parent/'als-cde-modular-prototype'
class ModularTests(unittest.TestCase):
    def test_inventory(self):
        r=ConceptRegistry(ROOT)
        self.assertEqual(len(r.load()),154)
        self.assertEqual(r.validate()['duplicateIds'],[])
    def test_external_codes(self):
        r=ConceptRegistry(ROOT)
        ril=next(x for x in r.load() if x['id']=='RILUZOLE')
        self.assertTrue(ril['compose']['include'])
        self.assertGreater(len(ril['compose']['include'][0]['filter'][0]['value']),20)
    def test_ranges(self):
        self.assertEqual(expand_range('E0100-E0102'),['E0100','E0101','E0102'])
        self.assertEqual(expand_range('001-003'),['001','002','003'])
    def test_query_medication(self):
        q=QueryFromJson(ROOT,'snow','source_code',['event_date'],['person_id'],'db.medications',
                        sel_keys=['RILUZOLE'],sel_domain='rx',cdtype_field='code_system',
                        cdtype_map={'rxnorm':'RX','ndc':'NDC'})
        sql=q.gen_qry()
        self.assertIn("code_system = 'RX'",sql)
        self.assertIn("code_system = 'NDC'",sql)
        self.assertIn("'RILUZOLE' AS CD_GRP",sql)
        self.assertIn('UNION ALL' if False else 'WHERE',sql)
    def test_query_observation(self):
        q=QueryFromJson(ROOT,'postgres','code',['obs_date'],['patient_id'],'observations',
          sel_keys=['ALSFRSR-Q1-SPEECH'],sel_domain='lab',cdtype_field='system',cdtype_map={'loinc':'LN'})
        sql=q.gen_qry()
        self.assertIn('82940-8',sql)
        self.assertIn('ALSFRSR-Q1-SPEECH',sql)
    def test_add_and_reindex(self):
        with tempfile.TemporaryDirectory() as d:
            root=Path(d);(root/'concepts').mkdir()
            c={'id':'ALS-NEW-TEST','name':'New test','description':'test',
               'relatedArtifact':{'class':'core','valueType':'boolean','valueRange':{}},
               'compose':{'include':[{'system':'loinc','concept':[{'code':'1234-5'}]}]}}
            r=ConceptRegistry(root)
            p=r.add(c,'diagnostic-testing')
            self.assertTrue(p.exists())
            self.assertEqual(len(r.load()),1)
            with self.assertRaises(ValueError):r.add(c,'diagnostic-testing')
            self.assertEqual(r.validate()['count'],1)
    def test_csv(self):
        with tempfile.TemporaryDirectory() as d:
            output=Path(d)/'codes.csv'
            json2ref(ROOT,output)
            self.assertTrue(output.exists())
            self.assertIn('RILUZOLE',output.read_text())
    def test_legacy(self):
        source=Path(__file__).parent/'vs-cde-als(1).json'
        if source.exists():self.assertEqual(len(ConceptRegistry(source).load()),154)

if __name__=='__main__':unittest.main()
